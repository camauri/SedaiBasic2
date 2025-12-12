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
unit SedaiSDL2VideoModes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SDL2;

type
  TSDL2VideoModeInfo = record
    Index: Integer;
    Width: Integer;
    Height: Integer;
    RefreshRate: Integer;
    PixelFormat: UInt32;
    AspectRatio: Double;
    AspectName: string;
    IsNative: Boolean;           // True if this is the desktop native mode
    MatchesDesktopAspect: Boolean; // True if aspect ratio matches desktop
  end;

  TSDL2VideoModeArray = array of TSDL2VideoModeInfo;

  { TSDL2VideoModeEnumerator }
  TSDL2VideoModeEnumerator = class
  private
    FModes: TSDL2VideoModeArray;
    FDisplayIndex: Integer;
    FCurrentDesktopMode: TSDL_DisplayMode;
    FInitialized: Boolean;

    function GetAspectRatioName(Width, Height: Integer): string;
    function CalculateAspectRatio(Width, Height: Integer): Double;
    procedure SortModesByResolution;
    procedure RemoveDuplicates;

  public
    constructor Create(DisplayIndex: Integer = 0);
    destructor Destroy; override;

    function EnumerateModes: Boolean;
    function GetModeCount: Integer;
    function GetMode(Index: Integer): TSDL2VideoModeInfo;
    function FindModeByResolution(Width, Height: Integer): Integer;
    function FindClosestMode(Width, Height: Integer): Integer;

    function GetDesktopWidth: Integer;
    function GetDesktopHeight: Integer;
    function GetDesktopRefreshRate: Integer;

    function GetModesAsStringList: TStringList;
    function FormatModeInfo(const Mode: TSDL2VideoModeInfo): string;

    property Modes: TSDL2VideoModeArray read FModes;
    property ModeCount: Integer read GetModeCount;
    property Initialized: Boolean read FInitialized;
  end;

implementation

{ TSDL2VideoModeEnumerator }

constructor TSDL2VideoModeEnumerator.Create(DisplayIndex: Integer);
begin
  inherited Create;
  FDisplayIndex := DisplayIndex;
  FInitialized := False;
  SetLength(FModes, 0);
end;

destructor TSDL2VideoModeEnumerator.Destroy;
begin
  SetLength(FModes, 0);
  inherited Destroy;
end;

function TSDL2VideoModeEnumerator.CalculateAspectRatio(Width, Height: Integer): Double;
begin
  if Height > 0 then
    Result := Width / Height
  else
    Result := 0;
end;

function TSDL2VideoModeEnumerator.GetAspectRatioName(Width, Height: Integer): string;
var
  Ratio: Double;
  GCD, W, H: Integer;
begin
  // Calculate GCD for simplified ratio
  W := Width;
  H := Height;
  while H <> 0 do
  begin
    GCD := H;
    H := W mod H;
    W := GCD;
  end;
  GCD := W;

  W := Width div GCD;
  H := Height div GCD;

  // Common aspect ratios
  Ratio := CalculateAspectRatio(Width, Height);

  if Abs(Ratio - 1.333) < 0.01 then
    Result := '4:3'
  else if Abs(Ratio - 1.6) < 0.01 then
    Result := '16:10'
  else if Abs(Ratio - 1.777) < 0.02 then
    Result := '16:9'
  else if Abs(Ratio - 1.25) < 0.01 then
    Result := '5:4'
  else if Abs(Ratio - 2.333) < 0.02 then
    Result := '21:9'
  else if Abs(Ratio - 3.555) < 0.02 then
    Result := '32:9'
  else
    Result := Format('%d:%d', [W, H]);
end;

function TSDL2VideoModeEnumerator.EnumerateModes: Boolean;
var
  NumModes, i: Integer;
  DisplayMode: TSDL_DisplayMode;
  ModeInfo: TSDL2VideoModeInfo;
begin
  Result := False;
  SetLength(FModes, 0);

  // Ensure SDL2 video is initialized
  if SDL_WasInit(SDL_INIT_VIDEO) = 0 then
  begin
    if SDL_Init(SDL_INIT_VIDEO) < 0 then
      Exit;
  end;

  // Get desktop mode
  if SDL_GetDesktopDisplayMode(FDisplayIndex, @FCurrentDesktopMode) < 0 then
    Exit;

  // Get number of display modes
  NumModes := SDL_GetNumDisplayModes(FDisplayIndex);
  if NumModes < 1 then
    Exit;

  // Enumerate all modes
  for i := 0 to NumModes - 1 do
  begin
    if SDL_GetDisplayMode(FDisplayIndex, i, @DisplayMode) = 0 then
    begin
      ModeInfo.Index := i;
      ModeInfo.Width := DisplayMode.w;
      ModeInfo.Height := DisplayMode.h;
      ModeInfo.RefreshRate := DisplayMode.refresh_rate;
      ModeInfo.PixelFormat := DisplayMode.format;
      ModeInfo.AspectRatio := CalculateAspectRatio(DisplayMode.w, DisplayMode.h);
      ModeInfo.AspectName := GetAspectRatioName(DisplayMode.w, DisplayMode.h);
      // Mark as native if matches desktop resolution
      ModeInfo.IsNative := (DisplayMode.w = FCurrentDesktopMode.w) and
                           (DisplayMode.h = FCurrentDesktopMode.h);
      // Mark if aspect ratio matches desktop (within 1% tolerance)
      ModeInfo.MatchesDesktopAspect := Abs(ModeInfo.AspectRatio -
        CalculateAspectRatio(FCurrentDesktopMode.w, FCurrentDesktopMode.h)) < 0.02;

      SetLength(FModes, Length(FModes) + 1);
      FModes[High(FModes)] := ModeInfo;
    end;
  end;

  // Sort and remove duplicates (same resolution, different refresh rates)
  SortModesByResolution;
  RemoveDuplicates;

  FInitialized := Length(FModes) > 0;
  Result := FInitialized;
end;

procedure TSDL2VideoModeEnumerator.SortModesByResolution;
var
  i, j: Integer;
  Temp: TSDL2VideoModeInfo;
begin
  // Simple bubble sort by pixels (descending)
  for i := 0 to High(FModes) - 1 do
    for j := i + 1 to High(FModes) do
    begin
      if (FModes[j].Width * FModes[j].Height) > (FModes[i].Width * FModes[i].Height) then
      begin
        Temp := FModes[i];
        FModes[i] := FModes[j];
        FModes[j] := Temp;
      end;
    end;
end;

procedure TSDL2VideoModeEnumerator.RemoveDuplicates;
var
  i, j, UniqueCount: Integer;
  UniqueModes: TSDL2VideoModeArray;
  Found: Boolean;
begin
  SetLength(UniqueModes, 0);

  for i := 0 to High(FModes) do
  begin
    Found := False;
    for j := 0 to High(UniqueModes) do
    begin
      if (UniqueModes[j].Width = FModes[i].Width) and
         (UniqueModes[j].Height = FModes[i].Height) then
      begin
        // Keep the one with higher refresh rate
        if FModes[i].RefreshRate > UniqueModes[j].RefreshRate then
          UniqueModes[j] := FModes[i];
        Found := True;
        Break;
      end;
    end;

    if not Found then
    begin
      SetLength(UniqueModes, Length(UniqueModes) + 1);
      UniqueModes[High(UniqueModes)] := FModes[i];
    end;
  end;

  FModes := UniqueModes;

  // Re-index after removing duplicates
  for i := 0 to High(FModes) do
    FModes[i].Index := i;
end;

function TSDL2VideoModeEnumerator.GetModeCount: Integer;
begin
  Result := Length(FModes);
end;

function TSDL2VideoModeEnumerator.GetMode(Index: Integer): TSDL2VideoModeInfo;
begin
  if (Index >= 0) and (Index < Length(FModes)) then
    Result := FModes[Index]
  else
  begin
    FillChar(Result, SizeOf(Result), 0);
    Result.Index := -1;
  end;
end;

function TSDL2VideoModeEnumerator.FindModeByResolution(Width, Height: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FModes) do
  begin
    if (FModes[i].Width = Width) and (FModes[i].Height = Height) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TSDL2VideoModeEnumerator.FindClosestMode(Width, Height: Integer): Integer;
var
  i: Integer;
  TargetPixels, ModePixels, BestDiff, Diff: Int64;
begin
  Result := -1;
  if Length(FModes) = 0 then Exit;

  TargetPixels := Int64(Width) * Int64(Height);
  BestDiff := MaxInt;

  for i := 0 to High(FModes) do
  begin
    ModePixels := Int64(FModes[i].Width) * Int64(FModes[i].Height);
    Diff := Abs(ModePixels - TargetPixels);

    if Diff < BestDiff then
    begin
      BestDiff := Diff;
      Result := i;
    end;
  end;
end;

function TSDL2VideoModeEnumerator.GetDesktopWidth: Integer;
begin
  Result := FCurrentDesktopMode.w;
end;

function TSDL2VideoModeEnumerator.GetDesktopHeight: Integer;
begin
  Result := FCurrentDesktopMode.h;
end;

function TSDL2VideoModeEnumerator.GetDesktopRefreshRate: Integer;
begin
  Result := FCurrentDesktopMode.refresh_rate;
end;

function TSDL2VideoModeEnumerator.FormatModeInfo(const Mode: TSDL2VideoModeInfo): string;
var
  Marker: string;
begin
  if Mode.IsNative then
    Marker := '*'
  else if Mode.MatchesDesktopAspect then
    Marker := '+'
  else
    Marker := ' ';
  Result := Format('%s%3d: %4dx%-4d %3dHz %s',
    [Marker, Mode.Index, Mode.Width, Mode.Height, Mode.RefreshRate, Mode.AspectName]);
end;

function TSDL2VideoModeEnumerator.GetModesAsStringList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;

  Result.Add(Format('Desktop: %dx%d @%dHz',
    [GetDesktopWidth, GetDesktopHeight, GetDesktopRefreshRate]));
  Result.Add('');
  Result.Add('SDL2 video modes:');
  Result.Add('* = native, + = same aspect ratio');
  Result.Add('----------------------------------');

  for i := 0 to High(FModes) do
    Result.Add(FormatModeInfo(FModes[i]));
end;

end.
