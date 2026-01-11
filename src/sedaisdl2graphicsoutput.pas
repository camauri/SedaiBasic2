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
unit SedaiSDL2GraphicsOutput;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Math, StrUtils,
  SDL2, SDL2_ttf,
  SedaiOutputInterface, SedaiGraphicsModes,
  SedaiSDL2Output, SedaiGraphicsMemory;

type
  TDisplayMode = (dmWindowed, dmFullscreenNative, dmFullscreenOptimal);
  TScalingMode = (smExactFit, smBorder);

  TVideoResolution = record
    Width: Integer;
    Height: Integer;
    Name: string;
    AspectRatio: Double;
  end;

  { TSDL2GraphicsOutputDevice }
  TSDL2GraphicsOutputDevice = class(TSDL2OutputDevice)
  private
    // Graphics core
    FGraphicsMemory: TGraphicsMemory;
    FGraphicsTexture: PSDL_Texture;
    FPaletteManager: TPaletteManager;

    // Mode management
    FCurrentMode: TGraphicMode;
    FCurrentModeInfo: TGraphicModeInfo;
    FSplitLine: Integer;

    // State
    FInGraphicsMode: Boolean;
    FTextModeActive: Boolean;
    FCursorEnabled: Boolean;

    // Text buffer per transizioni
    FTextBuffer: PByte;
    FTextBufferSize: Integer;

    // Display configuration
    FMaxSupportedWidth: Integer;
    FMaxSupportedHeight: Integer;
    //FCalculatedFontSize: Integer;

    // Border and colors
    FSimulateBorder: Boolean;
    // NOTE: FViewportBackground/FViewportForeground removed - use inherited FForegroundColor/FBackgroundColor

    // Hires mode colors (captured at mode switch for 0/1 color interpretation)
    FHiresForegroundRGBA: UInt32;
    FHiresBackgroundRGBA: UInt32;

    // Display mode settings
    FDisplayMode: TDisplayMode;
    FScalingMode: TScalingMode;

    // Private methods
    procedure DetectMaxResolution;
    procedure CreateGraphicsTexture;
    procedure UpdateGraphicsTexture;
    procedure EnableTextCursor(Enable: Boolean);
    procedure SaveTextBuffer;
    procedure RestoreTextBuffer;
    procedure ApplyPaletteToMemory;
    procedure RenderSplitScreen;
    procedure RenderGraphicsMode;
    procedure RenderTextMode;
    procedure RenderBitmapMode;

  public
    constructor Create;
    destructor Destroy; override;

    function SetGraphicMode(Mode: TGraphicMode; ClearBuffer: Boolean = False;
                           SplitLine: Integer = -1): Boolean; reintroduce;
    function GetGraphicMode: TGraphicMode; override;
    function IsInGraphicsMode: Boolean; override;
    procedure Present; override;
    procedure Clear; override;

    procedure SetPixel(X, Y: Integer; RGB: UInt32); overload; override;
    procedure SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex); overload; override;
    function GetPixel(X, Y: Integer): UInt32; override;

    procedure EnablePalette(Enable: Boolean); override;
    function IsPaletteEnabled: Boolean; override;
    procedure SetPaletteColor(Index: TPaletteIndex; RGB: UInt32); override;
    function GetPaletteColor(Index: TPaletteIndex): UInt32; override;
    procedure ResetPalette; override;

    // Override DrawBoxStyled to draw into graphics memory instead of renderer
    procedure DrawBoxStyled(X1, Y1, X2, Y2: Integer; const Style: TShapeStyle; Angle: Double = 0); override;
    // Override DrawBoxWithColor to interpret colors based on graphics mode
    procedure DrawBoxWithColor(X1, Y1, X2, Y2: Integer; Color: UInt32; Angle: Double = 0; Filled: Boolean = False); override;

    procedure SetDisplayPreferences(DisplayMode: TDisplayMode; ScalingMode: TScalingMode);

    function GetOptimalCols: Integer;
    function GetOptimalRows: Integer;
    function GetOptimalWidth: Integer;
    function GetOptimalHeight: Integer;
    function GetOptimalWindowWidth: Integer;
    function GetOptimalWindowHeight: Integer;

    property CurrentMode: TGraphicMode read FCurrentMode;
    property CurrentModeInfo: TGraphicModeInfo read FCurrentModeInfo;
    property NativeWidth: Integer read FCurrentModeInfo.NativeWidth;
    property NativeHeight: Integer read FCurrentModeInfo.NativeHeight;
    property PaletteManager: TPaletteManager read FPaletteManager;
    property GraphicsMemory: TGraphicsMemory read FGraphicsMemory;
    property SimulateBorder: Boolean read FSimulateBorder write FSimulateBorder;
    property MaxSupportedWidth: Integer read FMaxSupportedWidth;
    property MaxSupportedHeight: Integer read FMaxSupportedHeight;
    // ViewportBackground/ViewportForeground removed - use inherited BackgroundColor/ForegroundColor
    property DisplayMode: TDisplayMode read FDisplayMode;
    property ScalingMode: TScalingMode read FScalingMode;
  end;

const
  // Risoluzioni 4:3 (1.333)
  VIDEO_RESOLUTIONS_4_3: array[0..7] of TVideoResolution = (
    (Width: 640; Height: 480; Name: 'VGA'; AspectRatio: 1.333),
    (Width: 800; Height: 600; Name: 'SVGA'; AspectRatio: 1.333),
    (Width: 1024; Height: 768; Name: 'XGA'; AspectRatio: 1.333),
    (Width: 1152; Height: 864; Name: 'XGA+'; AspectRatio: 1.333),
    (Width: 1280; Height: 960; Name: 'SXGA-'; AspectRatio: 1.333),
    (Width: 1280; Height: 1024; Name: 'SXGA'; AspectRatio: 1.25),
    (Width: 1600; Height: 1200; Name: 'UXGA'; AspectRatio: 1.333),
    (Width: 2048; Height: 1536; Name: 'QXGA'; AspectRatio: 1.333)
  );

var
  // Risoluzioni 16:9 (1.778)
  VIDEO_RESOLUTIONS_16_9: array[0..9] of TVideoResolution;

  // Risoluzioni 16:10 (1.6)
  VIDEO_RESOLUTIONS_16_10: array[0..6] of TVideoResolution;

  // Risoluzioni non standard (ultrawide, industriali, ecc.)
  VIDEO_RESOLUTIONS_NONSTANDARD: array[0..7] of TVideoResolution;

implementation

constructor TSDL2GraphicsOutputDevice.Create;
var
  BestModeInfo: TGraphicModeInfo;
begin
  // Inizializza array risoluzioni
  VIDEO_RESOLUTIONS_16_9[0].Width := 1024; VIDEO_RESOLUTIONS_16_9[0].Height := 576; VIDEO_RESOLUTIONS_16_9[0].Name := 'WSVGA'; VIDEO_RESOLUTIONS_16_9[0].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[1].Width := 1280; VIDEO_RESOLUTIONS_16_9[1].Height := 720; VIDEO_RESOLUTIONS_16_9[1].Name := 'HD 720p'; VIDEO_RESOLUTIONS_16_9[1].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[2].Width := 1366; VIDEO_RESOLUTIONS_16_9[2].Height := 768; VIDEO_RESOLUTIONS_16_9[2].Name := 'WXGA'; VIDEO_RESOLUTIONS_16_9[2].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[3].Width := 1600; VIDEO_RESOLUTIONS_16_9[3].Height := 900; VIDEO_RESOLUTIONS_16_9[3].Name := 'HD+'; VIDEO_RESOLUTIONS_16_9[3].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[4].Width := 1920; VIDEO_RESOLUTIONS_16_9[4].Height := 1080; VIDEO_RESOLUTIONS_16_9[4].Name := 'Full HD'; VIDEO_RESOLUTIONS_16_9[4].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[5].Width := 2560; VIDEO_RESOLUTIONS_16_9[5].Height := 1440; VIDEO_RESOLUTIONS_16_9[5].Name := 'QHD'; VIDEO_RESOLUTIONS_16_9[5].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[6].Width := 3200; VIDEO_RESOLUTIONS_16_9[6].Height := 1800; VIDEO_RESOLUTIONS_16_9[6].Name := 'QHD+'; VIDEO_RESOLUTIONS_16_9[6].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[7].Width := 3840; VIDEO_RESOLUTIONS_16_9[7].Height := 2160; VIDEO_RESOLUTIONS_16_9[7].Name := '4K UHD'; VIDEO_RESOLUTIONS_16_9[7].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[8].Width := 5120; VIDEO_RESOLUTIONS_16_9[8].Height := 2880; VIDEO_RESOLUTIONS_16_9[8].Name := '5K'; VIDEO_RESOLUTIONS_16_9[8].AspectRatio := 1.778;
  VIDEO_RESOLUTIONS_16_9[9].Width := 7680; VIDEO_RESOLUTIONS_16_9[9].Height := 4320; VIDEO_RESOLUTIONS_16_9[9].Name := '8K UHD'; VIDEO_RESOLUTIONS_16_9[9].AspectRatio := 1.778;

  VIDEO_RESOLUTIONS_16_10[0].Width := 1280; VIDEO_RESOLUTIONS_16_10[0].Height := 800; VIDEO_RESOLUTIONS_16_10[0].Name := 'WXGA'; VIDEO_RESOLUTIONS_16_10[0].AspectRatio := 1.6;
  VIDEO_RESOLUTIONS_16_10[1].Width := 1440; VIDEO_RESOLUTIONS_16_10[1].Height := 900; VIDEO_RESOLUTIONS_16_10[1].Name := 'WXGA+'; VIDEO_RESOLUTIONS_16_10[1].AspectRatio := 1.6;
  VIDEO_RESOLUTIONS_16_10[2].Width := 1680; VIDEO_RESOLUTIONS_16_10[2].Height := 1050; VIDEO_RESOLUTIONS_16_10[2].Name := 'WSXGA+'; VIDEO_RESOLUTIONS_16_10[2].AspectRatio := 1.6;
  VIDEO_RESOLUTIONS_16_10[3].Width := 1920; VIDEO_RESOLUTIONS_16_10[3].Height := 1200; VIDEO_RESOLUTIONS_16_10[3].Name := 'WUXGA'; VIDEO_RESOLUTIONS_16_10[3].AspectRatio := 1.6;
  VIDEO_RESOLUTIONS_16_10[4].Width := 2560; VIDEO_RESOLUTIONS_16_10[4].Height := 1600; VIDEO_RESOLUTIONS_16_10[4].Name := 'WQXGA'; VIDEO_RESOLUTIONS_16_10[4].AspectRatio := 1.6;
  VIDEO_RESOLUTIONS_16_10[5].Width := 3840; VIDEO_RESOLUTIONS_16_10[5].Height := 2400; VIDEO_RESOLUTIONS_16_10[5].Name := '4K WUXGA'; VIDEO_RESOLUTIONS_16_10[5].AspectRatio := 1.6;
  VIDEO_RESOLUTIONS_16_10[6].Width := 5120; VIDEO_RESOLUTIONS_16_10[6].Height := 3200; VIDEO_RESOLUTIONS_16_10[6].Name := '5K WUXGA'; VIDEO_RESOLUTIONS_16_10[6].AspectRatio := 1.6;

  VIDEO_RESOLUTIONS_NONSTANDARD[0].Width := 2560; VIDEO_RESOLUTIONS_NONSTANDARD[0].Height := 1080; VIDEO_RESOLUTIONS_NONSTANDARD[0].Name := 'UW-FHD'; VIDEO_RESOLUTIONS_NONSTANDARD[0].AspectRatio := 2.370;
  VIDEO_RESOLUTIONS_NONSTANDARD[1].Width := 3440; VIDEO_RESOLUTIONS_NONSTANDARD[1].Height := 1440; VIDEO_RESOLUTIONS_NONSTANDARD[1].Name := 'UW-QHD'; VIDEO_RESOLUTIONS_NONSTANDARD[1].AspectRatio := 2.389;
  VIDEO_RESOLUTIONS_NONSTANDARD[2].Width := 3840; VIDEO_RESOLUTIONS_NONSTANDARD[2].Height := 1600; VIDEO_RESOLUTIONS_NONSTANDARD[2].Name := 'UW-QHD+'; VIDEO_RESOLUTIONS_NONSTANDARD[2].AspectRatio := 2.4;
  VIDEO_RESOLUTIONS_NONSTANDARD[3].Width := 5120; VIDEO_RESOLUTIONS_NONSTANDARD[3].Height := 2160; VIDEO_RESOLUTIONS_NONSTANDARD[3].Name := 'UW-5K'; VIDEO_RESOLUTIONS_NONSTANDARD[3].AspectRatio := 2.370;
  VIDEO_RESOLUTIONS_NONSTANDARD[4].Width := 1280; VIDEO_RESOLUTIONS_NONSTANDARD[4].Height := 1024; VIDEO_RESOLUTIONS_NONSTANDARD[4].Name := 'SXGA 5:4'; VIDEO_RESOLUTIONS_NONSTANDARD[4].AspectRatio := 1.25;
  VIDEO_RESOLUTIONS_NONSTANDARD[5].Width := 1600; VIDEO_RESOLUTIONS_NONSTANDARD[5].Height := 1024; VIDEO_RESOLUTIONS_NONSTANDARD[5].Name := 'WSXGA 25:16'; VIDEO_RESOLUTIONS_NONSTANDARD[5].AspectRatio := 1.5625;
  VIDEO_RESOLUTIONS_NONSTANDARD[6].Width := 2048; VIDEO_RESOLUTIONS_NONSTANDARD[6].Height := 1080; VIDEO_RESOLUTIONS_NONSTANDARD[6].Name := 'DCI 2K'; VIDEO_RESOLUTIONS_NONSTANDARD[6].AspectRatio := 1.896;
  VIDEO_RESOLUTIONS_NONSTANDARD[7].Width := 4096; VIDEO_RESOLUTIONS_NONSTANDARD[7].Height := 2160; VIDEO_RESOLUTIONS_NONSTANDARD[7].Name := 'DCI 4K'; VIDEO_RESOLUTIONS_NONSTANDARD[7].AspectRatio := 1.896;

  FGraphicsMemory := TGraphicsMemory.Create;
  FGraphicsTexture := nil;
  FTextBuffer := nil;
  FTextBufferSize := 0;
  FSimulateBorder := True;

  DetectMaxResolution;

  {$IFDEF WINDOWS}
  FDisplayMode := dmWindowed;
  {$ELSE}
  FDisplayMode := dmFullscreenNative;
  {$ENDIF}

  FScalingMode := smBorder;

  // NOTE: FViewportBackground/FViewportForeground removed
  // Colors are now inherited from TSDL2OutputDevice (FForegroundColor/FBackgroundColor)

  BestModeInfo := GetModeInfo(gm80x50Text);
  FCurrentMode := BestModeInfo.Mode;
  FCurrentModeInfo := BestModeInfo;

  case BestModeInfo.ModeType of
    mtText:   begin FTextModeActive := True;  FInGraphicsMode := False; end;
    mtBitmap: begin FTextModeActive := False; FInGraphicsMode := True;  end;
    mtMixed:  begin FTextModeActive := True;  FInGraphicsMode := True;  end;
  end;

  inherited Create;

  SetDisplayParameters(GetOptimalWindowWidth, GetOptimalWindowHeight,
                      BestModeInfo.TextCols, BestModeInfo.TextRows,
                      BestModeInfo.NativeWidth, BestModeInfo.NativeHeight);

  FPaletteManager := TPaletteManager.Create(ptC64_16);

  if IsInitialized then
  begin
    CreateGraphicsTexture;
  end;
end;

destructor TSDL2GraphicsOutputDevice.Destroy;
begin
  if Assigned(FTextBuffer) then
    FreeMem(FTextBuffer);

  if Assigned(FGraphicsTexture) then
    SDL_DestroyTexture(FGraphicsTexture);

  FPaletteManager.Free;
  FGraphicsMemory.Free;

  inherited Destroy;
end;

procedure TSDL2GraphicsOutputDevice.DetectMaxResolution;
var
  ADisplayMode: TSDL_DisplayMode;
  NumDisplays: Integer;
begin
  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    FMaxSupportedWidth := 1024;
    FMaxSupportedHeight := 768;
    Exit;
  end;

  NumDisplays := SDL_GetNumVideoDisplays;
  if NumDisplays > 0 then
  begin
    if SDL_GetDesktopDisplayMode(0, @ADisplayMode) = 0 then
    begin
      FMaxSupportedWidth := ADisplayMode.w;
      FMaxSupportedHeight := ADisplayMode.h;
    end
    else
    begin
      FMaxSupportedWidth := 1920;
      FMaxSupportedHeight := 1080;
    end;
  end
  else
  begin
    FMaxSupportedWidth := 1024;
    FMaxSupportedHeight := 768;
  end;
end;

function TSDL2GraphicsOutputDevice.SetGraphicMode(Mode: TGraphicMode;
                                                 ClearBuffer: Boolean = False;
                                                 SplitLine: Integer = -1): Boolean;
var
 NewModeInfo: TGraphicModeInfo;
begin
 Result := False;

 NewModeInfo := GetModeInfo(Mode);

 if IsInitialized and FTextModeActive and (NewModeInfo.ModeType <> mtText) then
 begin
   SaveTextBuffer;
 end;

 FCurrentMode := Mode;
 FCurrentModeInfo := NewModeInfo;
 FSplitLine := SplitLine;
 if FSplitLine < 0 then
   FSplitLine := NewModeInfo.DefaultSplitLine;

 case NewModeInfo.ModeType of
   mtText:
   begin
     FTextModeActive := True;
     FInGraphicsMode := False;
     if IsInitialized then
     begin
       if Assigned(FTextBuffer) then
         RestoreTextBuffer;
       // Show mouse cursor in text mode
       SDL_ShowCursor(SDL_ENABLE);
     end;
   end;
   mtBitmap:
   begin
     FTextModeActive := False;
     FInGraphicsMode := True;
     // Capture current foreground/background colors for hires mode color interpretation
     // Color 1 = foreground, Color 0 = background
     // Use inherited FForegroundColor/FBackgroundColor from TSDL2OutputDevice
     FHiresForegroundRGBA := RGBA(FForegroundColor.R, FForegroundColor.G, FForegroundColor.B, 255);
     FHiresBackgroundRGBA := RGBA(FBackgroundColor.R, FBackgroundColor.G, FBackgroundColor.B, 255);
     if IsInitialized then
     begin
       inherited HideCursor(inherited GetCursorX, inherited GetCursorY);
       // Hide mouse cursor in bitmap mode
       SDL_ShowCursor(SDL_DISABLE);
     end;
   end;
   mtMixed:
   begin
     FTextModeActive := True;
     FInGraphicsMode := True;
   end;
 end;

 // If already initialized, reconfigure the display
 if IsInitialized then
   SetDisplayPreferences(FDisplayMode, FScalingMode);

 if IsInitialized and ClearBuffer then
   Clear;

 Result := True;
end;

function TSDL2GraphicsOutputDevice.GetGraphicMode: TGraphicMode;
begin
  Result := FCurrentMode;
end;

procedure TSDL2GraphicsOutputDevice.SaveTextBuffer;
var
  BufferSize: Integer;
begin
  if FTextBufferSize > 0 then
    FreeMem(FTextBuffer);

  BufferSize := GetActualCols * GetActualRows * 4;
  FTextBufferSize := BufferSize;
  GetMem(FTextBuffer, FTextBufferSize);

  FGraphicsMemory.SaveTextFrame(FTextBuffer, FTextBufferSize);
end;

procedure TSDL2GraphicsOutputDevice.RestoreTextBuffer;
begin
  if not Assigned(FTextBuffer) then Exit;

  FGraphicsMemory.RestoreTextFrame(FTextBuffer, FTextBufferSize);

  FreeMem(FTextBuffer);
  FTextBuffer := nil;
  FTextBufferSize := 0;

  inherited Clear;
  Present;
end;

procedure TSDL2GraphicsOutputDevice.ApplyPaletteToMemory;
var
  i: Integer;
begin
  for i := 0 to 255 do
  begin
    FGraphicsMemory.SetPaletteColor(i, FPaletteManager.GetColor(i));
  end;
end;

procedure TSDL2GraphicsOutputDevice.EnableTextCursor(Enable: Boolean);
begin
  FCursorEnabled := Enable;
  if not Enable then
    inherited HideCursor(inherited GetCursorX, inherited GetCursorY);
end;

procedure TSDL2GraphicsOutputDevice.CreateGraphicsTexture;
begin
  if Assigned(FGraphicsTexture) then
    SDL_DestroyTexture(FGraphicsTexture);

  FGraphicsTexture := SDL_CreateTexture(FRenderer, SDL_PIXELFORMAT_RGBA8888,
    SDL_TEXTUREACCESS_STREAMING,
    FGraphicsMemory.State.Width, FGraphicsMemory.State.Height);
end;

procedure TSDL2GraphicsOutputDevice.UpdateGraphicsTexture;
var
  Pixels: PByte;
  Pitch: Integer;
begin
  if not Assigned(FGraphicsTexture) then Exit;

  if SDL_LockTexture(FGraphicsTexture, nil, @Pixels, @Pitch) = 0 then
  begin
    try
      Move(FGraphicsMemory.GraphicsBuffer^, Pixels^,
           FGraphicsMemory.State.Width * FGraphicsMemory.State.Height * 4);
    finally
      SDL_UnlockTexture(FGraphicsTexture);
    end;
  end;
end;

procedure TSDL2GraphicsOutputDevice.RenderTextMode;
begin
  inherited Present;
end;

procedure TSDL2GraphicsOutputDevice.RenderBitmapMode;
begin
  if Assigned(FGraphicsTexture) then
  begin
    UpdateGraphicsTexture;
    SDL_RenderCopy(FRenderer, FGraphicsTexture, nil, nil);
  end;
end;

procedure TSDL2GraphicsOutputDevice.RenderSplitScreen;
begin
  // Implementation for split screen rendering
end;

procedure TSDL2GraphicsOutputDevice.RenderGraphicsMode;
begin
  if FCurrentModeInfo.ModeType = mtMixed then
    RenderSplitScreen
  else
    RenderBitmapMode;
end;

function TSDL2GraphicsOutputDevice.IsInGraphicsMode: Boolean;
begin
  Result := FInGraphicsMode;
end;

procedure TSDL2GraphicsOutputDevice.Present;
begin
  if not IsInitialized then Exit;

  if FTextModeActive and not FInGraphicsMode then
  begin
    inherited Present;
    Exit;
  end;

  if Assigned(FGraphicsTexture) then
  begin
    UpdateGraphicsTexture;
    SDL_RenderCopy(FRenderer, FGraphicsTexture, nil, nil);
  end;

  if (FCurrentModeInfo.ModeType = mtMixed) and FTextModeActive then
  begin
    inherited Present;
  end;

  SDL_RenderPresent(FRenderer);
end;

procedure TSDL2GraphicsOutputDevice.Clear;
begin
  if FInGraphicsMode then
  begin
    FGraphicsMemory.ClearGraphicsBuffer;
    FGraphicsMemory.ClearColorBuffer;
  end
  else
  begin
    inherited Clear;
  end;
end;

procedure TSDL2GraphicsOutputDevice.SetPixel(X, Y: Integer; RGB: UInt32);
begin
  if (X < 0) or (Y < 0) or
     (X >= FCurrentModeInfo.NativeWidth) or
     (Y >= FCurrentModeInfo.NativeHeight) then
    Exit;

  FGraphicsMemory.SetPixel(X, Y, RGB);
end;

procedure TSDL2GraphicsOutputDevice.SetPixel(X, Y: Integer; PaletteIndex: TPaletteIndex);
begin
  if (X < 0) or (Y < 0) or
     (X >= FCurrentModeInfo.NativeWidth) or
     (Y >= FCurrentModeInfo.NativeHeight) then
    Exit;

  if FCurrentModeInfo.PaletteType = ptC64_16 then
    PaletteIndex := PaletteIndex mod 16;

  FGraphicsMemory.SetPixel(X, Y, PaletteIndex);
end;

function TSDL2GraphicsOutputDevice.GetPixel(X, Y: Integer): UInt32;
begin
  Result := FGraphicsMemory.GetPixel(X, Y);
end;

procedure TSDL2GraphicsOutputDevice.EnablePalette(Enable: Boolean);
begin
  FGraphicsMemory.EnablePalette(Enable);
end;

function TSDL2GraphicsOutputDevice.IsPaletteEnabled: Boolean;
begin
  Result := FGraphicsMemory.IsPaletteEnabled;
end;

procedure TSDL2GraphicsOutputDevice.SetPaletteColor(Index: TPaletteIndex; RGB: UInt32);
begin
  FPaletteManager.SetColor(Index, RGB);
  FGraphicsMemory.SetPaletteColor(Index, RGB);
end;

function TSDL2GraphicsOutputDevice.GetPaletteColor(Index: TPaletteIndex): UInt32;
begin
  Result := FPaletteManager.GetColor(Index);
end;

procedure TSDL2GraphicsOutputDevice.ResetPalette;
begin
  FPaletteManager.LoadDefaultPalette;
  ApplyPaletteToMemory;
end;

procedure TSDL2GraphicsOutputDevice.SetDisplayPreferences(DisplayMode: TDisplayMode;
                                                         ScalingMode: TScalingMode);
var
  OptimalScale: Integer;
  ViewportWidth, ViewportHeight: Integer;
  WindowWidth, WindowHeight: Integer;
  AspectRatioVideo, AspectRatioNative: Double;
  i: Integer;
  BestResolution: TVideoResolution;

  function FindBestResolution(NativeW, NativeH: Integer; ScalingMode: TScalingMode): TVideoResolution;
  var
    i: Integer;
    BestFit: TVideoResolution;
    MonitorW, MonitorH: Integer;
  begin
    MonitorW := FMaxSupportedWidth;
    MonitorH := FMaxSupportedHeight;

    BestFit.Width := 0;
    BestFit.Height := 0;
    BestFit.Name := '';

    // Determina quale array usare tramite moltiplicazione
    if (MonitorW * 3) = (MonitorH * 4) then
    begin
      // Aspect ratio 4:3
      for i := 0 to 7 do
      begin
        if (VIDEO_RESOLUTIONS_4_3[i].Width >= NativeW) and (VIDEO_RESOLUTIONS_4_3[i].Height >= NativeH) then
        begin
          if ScalingMode = smBorder then
          begin
            if (VIDEO_RESOLUTIONS_4_3[i].Width > NativeW) and (VIDEO_RESOLUTIONS_4_3[i].Height > NativeH) then
            begin
              BestFit := VIDEO_RESOLUTIONS_4_3[i];
              Break;
            end;
          end
          else
          begin
            BestFit := VIDEO_RESOLUTIONS_4_3[i];
            Break;
          end;
        end;
      end;
    end
    else if (MonitorW * 9) = (MonitorH * 16) then
    begin
      // Aspect ratio 16:9
      for i := 0 to 9 do
      begin
        if (VIDEO_RESOLUTIONS_16_9[i].Width >= NativeW) and (VIDEO_RESOLUTIONS_16_9[i].Height >= NativeH) then
        begin
          if ScalingMode = smBorder then
          begin
            if (VIDEO_RESOLUTIONS_16_9[i].Width > NativeW) and (VIDEO_RESOLUTIONS_16_9[i].Height > NativeH) then
            begin
              BestFit := VIDEO_RESOLUTIONS_16_9[i];
              Break;
            end;
          end
          else
          begin
            BestFit := VIDEO_RESOLUTIONS_16_9[i];
            Break;
          end;
        end;
      end;
    end
    else if (MonitorW * 10) = (MonitorH * 16) then
    begin
      // Aspect ratio 16:10
      for i := 0 to 6 do
      begin
        if (VIDEO_RESOLUTIONS_16_10[i].Width >= NativeW) and (VIDEO_RESOLUTIONS_16_10[i].Height >= NativeH) then
        begin
          if ScalingMode = smBorder then
          begin
            if (VIDEO_RESOLUTIONS_16_10[i].Width > NativeW) and (VIDEO_RESOLUTIONS_16_10[i].Height > NativeH) then
            begin
              BestFit := VIDEO_RESOLUTIONS_16_10[i];
              Break;
            end;
          end
          else
          begin
            BestFit := VIDEO_RESOLUTIONS_16_10[i];
            Break;
          end;
        end;
      end;
    end
    else
    begin
      // Aspect ratio non standard
      for i := 0 to 7 do
      begin
        if (VIDEO_RESOLUTIONS_NONSTANDARD[i].Width >= NativeW) and (VIDEO_RESOLUTIONS_NONSTANDARD[i].Height >= NativeH) then
        begin
          if ScalingMode = smBorder then
          begin
            if (VIDEO_RESOLUTIONS_NONSTANDARD[i].Width > NativeW) and (VIDEO_RESOLUTIONS_NONSTANDARD[i].Height > NativeH) then
            begin
              BestFit := VIDEO_RESOLUTIONS_NONSTANDARD[i];
              Break;
            end;
          end
          else
          begin
            BestFit := VIDEO_RESOLUTIONS_NONSTANDARD[i];
            Break;
          end;
        end;
      end;
    end;

    if BestFit.Width = 0 then
    begin
      BestFit.Width := MonitorW;
      BestFit.Height := MonitorH;
      BestFit.Name := 'Monitor Native';
      BestFit.AspectRatio := MonitorW / MonitorH;
    end;

    Result := BestFit;
  end;

begin
  FDisplayMode := DisplayMode;
  FScalingMode := ScalingMode;

  if IsInitialized then
  begin
    case DisplayMode of
      dmWindowed:
        begin
          ViewportWidth := FCurrentModeInfo.NativeWidth;
          ViewportHeight := FCurrentModeInfo.NativeHeight;
          WindowWidth := ViewportWidth;
          WindowHeight := ViewportHeight;
          OptimalScale := 1;
        end;

      dmFullscreenOptimal:
        begin
          WindowWidth := FMaxSupportedWidth;
          WindowHeight := FMaxSupportedHeight;

          AspectRatioVideo := WindowWidth / WindowHeight;
          AspectRatioNative := FCurrentModeInfo.NativeWidth / FCurrentModeInfo.NativeHeight;

          if AspectRatioVideo <= AspectRatioNative then
          begin
            OptimalScale := WindowHeight div FCurrentModeInfo.NativeHeight;
          end
          else
          begin
            OptimalScale := WindowWidth div FCurrentModeInfo.NativeWidth;
          end;

          if OptimalScale < 1 then OptimalScale := 1;

          ViewportWidth := FCurrentModeInfo.NativeWidth * OptimalScale;
          ViewportHeight := FCurrentModeInfo.NativeHeight * OptimalScale;

          if ScalingMode = smBorder then
          begin
            if (ViewportWidth = WindowWidth) or (ViewportHeight = WindowHeight) then
            begin
              Dec(OptimalScale);
              if OptimalScale < 1 then OptimalScale := 1;
              ViewportWidth := FCurrentModeInfo.NativeWidth * OptimalScale;
              ViewportHeight := FCurrentModeInfo.NativeHeight * OptimalScale;
            end;
          end;
        end;

      dmFullscreenNative:
        begin
          ViewportWidth := FCurrentModeInfo.NativeWidth;
          ViewportHeight := FCurrentModeInfo.NativeHeight;
          OptimalScale := 1;

          BestResolution := FindBestResolution(ViewportWidth, ViewportHeight, ScalingMode);
          WindowWidth := BestResolution.Width;
          WindowHeight := BestResolution.Height;
        end;
    end;

    SetDisplayParameters(WindowWidth, WindowHeight,
                        FCurrentModeInfo.TextCols, FCurrentModeInfo.TextRows,
                        FCurrentModeInfo.NativeWidth, FCurrentModeInfo.NativeHeight);

    //DesiredCols := FCurrentModeInfo.TextCols;
    //DesiredRows := FCurrentModeInfo.TextRows;
    //CalculatedFontSize := FCurrentModeInfo.NativeWidth div FCurrentModeInfo.TextCols;
    //FontSize := CalculatedFontSize;
    //LogicalWidth := FCurrentModeInfo.NativeWidth;
    //LogicalHeight := FCurrentModeInfo.NativeHeight;

    case DisplayMode of
      dmWindowed:
        begin
          SetFullscreen(False);
          SDL_SetWindowSize(SDL_RenderGetWindow(FRenderer), WindowWidth, WindowHeight);
          SDL_SetWindowPosition(SDL_RenderGetWindow(FRenderer), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);

          // Configura il logical size per il viewport
          SDL_RenderSetLogicalSize(FRenderer, ViewportWidth, ViewportHeight);
        end;

      dmFullscreenNative, dmFullscreenOptimal:
        begin
          SetFullscreen(True);

          if ScalingMode = smBorder then
          begin
            // Centered viewport with border
            SDL_RenderSetLogicalSize(FRenderer, ViewportWidth, ViewportHeight);
            // SDL automatically centers when logical size < window size
          end
          else // smExactFit
          begin
            // Viewport stretched to fill the entire screen
            SDL_RenderSetLogicalSize(FRenderer, FCurrentModeInfo.NativeWidth, FCurrentModeInfo.NativeHeight);
          end;
        end;
    end;

  end;
end;

function TSDL2GraphicsOutputDevice.GetOptimalCols: Integer;
begin
  Result := FCurrentModeInfo.TextCols;
  if Result = 0 then Result := 80;
end;

function TSDL2GraphicsOutputDevice.GetOptimalRows: Integer;
begin
  Result := FCurrentModeInfo.TextRows;
  if Result = 0 then Result := 25;
end;

function TSDL2GraphicsOutputDevice.GetOptimalWidth: Integer;
begin
  Result := FCurrentModeInfo.NativeWidth;
end;

function TSDL2GraphicsOutputDevice.GetOptimalHeight: Integer;
begin
  Result := FCurrentModeInfo.NativeHeight;
end;

function TSDL2GraphicsOutputDevice.GetOptimalWindowWidth: Integer;
begin
  case FDisplayMode of
    dmWindowed:
      Result := FCurrentModeInfo.NativeWidth * 2;
    dmFullscreenNative:
      Result := FMaxSupportedWidth;
    dmFullscreenOptimal:
      Result := ((FCurrentModeInfo.NativeWidth * 2 + 31) div 32) * 32;
    else
      Result := 1280;
  end;
end;

function TSDL2GraphicsOutputDevice.GetOptimalWindowHeight: Integer;
begin
  case FDisplayMode of
    dmWindowed:
      Result := FCurrentModeInfo.NativeHeight * 2;
    dmFullscreenNative:
      Result := FMaxSupportedHeight;
    dmFullscreenOptimal:
      Result := ((FCurrentModeInfo.NativeHeight * 2 + 31) div 32) * 32;
    else
      Result := 800;
  end;
end;

procedure TSDL2GraphicsOutputDevice.DrawBoxStyled(X1, Y1, X2, Y2: Integer;
  const Style: TShapeStyle; Angle: Double);
var
  MinX, MinY, MaxX, MaxY: Integer;
  X, Y: Integer;
  BorderColor, FillColor: UInt32;
begin
  // In graphics mode, we draw into the graphics memory buffer using SetPixel
  // This ensures the box appears in the graphics layer, not directly on the renderer

  // For now, only support non-rotated boxes (rotation would require more complex math)
  // TODO: Add rotation support for graphics memory drawing

  // Normalize coordinates
  MinX := Min(X1, X2);
  MaxX := Max(X1, X2);
  MinY := Min(Y1, Y2);
  MaxY := Max(Y1, Y2);

  // Clamp to graphics buffer bounds
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX >= FCurrentModeInfo.NativeWidth then MaxX := FCurrentModeInfo.NativeWidth - 1;
  if MaxY >= FCurrentModeInfo.NativeHeight then MaxY := FCurrentModeInfo.NativeHeight - 1;

  BorderColor := Style.Border.Color;
  FillColor := Style.Fill.Color;

  // Draw fill first (if any)
  if Style.Fill.Style <> fsNone then
  begin
    for Y := MinY + 1 to MaxY - 1 do
      for X := MinX + 1 to MaxX - 1 do
        FGraphicsMemory.SetPixel(X, Y, FillColor);
  end;

  // Draw border (if any)
  if (Style.Border.LineStyle <> blsNone) and (Style.Border.Width > 0) then
  begin
    // Top and bottom edges
    for X := MinX to MaxX do
    begin
      FGraphicsMemory.SetPixel(X, MinY, BorderColor);
      FGraphicsMemory.SetPixel(X, MaxY, BorderColor);
    end;
    // Left and right edges
    for Y := MinY to MaxY do
    begin
      FGraphicsMemory.SetPixel(MinX, Y, BorderColor);
      FGraphicsMemory.SetPixel(MaxX, Y, BorderColor);
    end;
  end;
end;

procedure TSDL2GraphicsOutputDevice.DrawBoxWithColor(X1, Y1, X2, Y2: Integer;
  Color: UInt32; Angle: Double; Filled: Boolean);
var
  Style: TShapeStyle;
  ActualColor: UInt32;
begin
  // Interpret color based on current graphics mode
  case FCurrentMode of
    gmStandardBitmap, gmSplitBitmap:  // Hires modes (GRAPHIC 1, 2)
    begin
      // In hires mode: 0 = background color, 1 = foreground color
      if Color = 0 then
        ActualColor := FHiresBackgroundRGBA
      else
        ActualColor := FHiresForegroundRGBA;
    end;
    gmMulticolorBitmap, gmSplitMulticolor:  // Multicolor modes (GRAPHIC 3, 4)
    begin
      // TODO: Implement multicolor palette interpretation
      // For now, use color as palette index 0-3
      ActualColor := Color;
    end;
    gmSDL2Dynamic:  // SDL2 truecolor mode (GRAPHIC 7)
    begin
      // Color is direct RGBA value
      ActualColor := Color;
    end;
    else
    begin
      // Text modes or unknown: use color directly
      ActualColor := Color;
    end;
  end;

  // Create style with the resolved color
  Style.Border := MakeDefaultBorderStyle(ActualColor, 1);
  if Filled then
  begin
    Style.Fill := MakeDefaultFillStyle(ActualColor);
    Style.Fill.Style := fsSolid;
  end
  else
  begin
    Style.Fill := MakeDefaultFillStyle(clTransparent);
    Style.Fill.Style := fsNone;
  end;

  // Call our local DrawBoxStyled that draws into graphics memory
  DrawBoxStyled(X1, Y1, X2, Y2, Style, Angle);
end;

end.
