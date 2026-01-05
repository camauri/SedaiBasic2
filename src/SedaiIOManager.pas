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
unit SedaiIOManager;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

{ ============================================================================
  SedaiIOManager - Central I/O factory for SedaiBasic

  This unit provides a unified factory to create I/O devices based on
  the desired mode:

  Modes:
  - ioTerminal:    Pure console I/O (no SDL2) - for command-line, batch, headless
  - ioRetroText:   SDL2 retro-style text console (40x25, C64-style)
  - ioRetroGfx:    SDL2 retro-style with graphics support (sprites, pixels)
  - ioModernText:  SDL2 modern text (80x50, larger resolution)
  - ioModernGfx:   SDL2 modern graphics (full resolution, truecolor)

  Usage:
    var
      IOManager: TIOManager;
      Output: IOutputDevice;
      Input: IInputDevice;
    begin
      IOManager := TIOManager.Create;
      try
        IOManager.SetMode(ioRetroText);
        Output := IOManager.CreateOutputDevice;
        Input := IOManager.CreateInputDevice;
        // Use Output and Input...
      finally
        IOManager.Free;
      end;
    end;
  ============================================================================ }

interface

uses
  Classes, SysUtils, SedaiOutputInterface;

type
  { I/O mode selection }
  TIOMode = (
    ioTerminal,     // Pure console (no SDL2 dependency)
    ioRetroText,    // SDL2 retro text console (40x25)
    ioRetroGfx,     // SDL2 retro with graphics
    ioModernText,   // SDL2 modern text (80x50)
    ioModernGfx     // SDL2 modern graphics (full resolution)
  );

  { TIOManager - Central factory for I/O devices }
  TIOManager = class
  private
    FMode: TIOMode;
    FTitle: string;
    FWidth: Integer;
    FHeight: Integer;
    FOutputDevice: IOutputDevice;
    FInputDevice: IInputDevice;
    FSDL2Available: Boolean;

    function IsSDL2Available: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Set the I/O mode }
    procedure SetMode(Mode: TIOMode);
    function GetMode: TIOMode;

    { Set window/console parameters (for SDL2 modes) }
    procedure SetWindowTitle(const Title: string);
    procedure SetConsoleSize(Width, Height: Integer);

    { Create devices based on current mode }
    function CreateOutputDevice: IOutputDevice;
    function CreateInputDevice: IInputDevice;

    { Get the current devices (creates if not exists) }
    function GetOutputDevice: IOutputDevice;
    function GetInputDevice: IInputDevice;

    { Utility }
    function ModeToString(Mode: TIOMode): string;
    function StringToMode(const S: string): TIOMode;

    { Properties }
    property Mode: TIOMode read FMode write SetMode;
    property Title: string read FTitle write SetWindowTitle;
    property ConsoleWidth: Integer read FWidth;
    property ConsoleHeight: Integer read FHeight;
    property SDL2Available: Boolean read IsSDL2Available;
  end;

  { Exception for I/O errors }
  EIOManagerError = class(Exception);

implementation

uses
  SedaiTerminalIO
  {$IFDEF HAS_SDL2}
  , SedaiOutputFactory, SedaiSDL2GraphicsOutput, SedaiSDL2Input
  {$ENDIF}
  ;

{ ============================================================================
  TIOManager
  ============================================================================ }

constructor TIOManager.Create;
begin
  inherited Create;
  FMode := ioTerminal;  // Default to terminal mode (safest)
  FTitle := 'SedaiBasic';
  FWidth := 80;
  FHeight := 25;
  FOutputDevice := nil;
  FInputDevice := nil;
  FSDL2Available := False;  // Will be checked on demand
end;

destructor TIOManager.Destroy;
begin
  // Interfaces are reference-counted, no need to free
  FOutputDevice := nil;
  FInputDevice := nil;
  inherited Destroy;
end;

function TIOManager.IsSDL2Available: Boolean;
begin
  // For now, assume SDL2 is available if we compiled with HAS_SDL2 define
  // In a more robust implementation, we could try to load SDL2 dynamically
  {$IFDEF HAS_SDL2}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TIOManager.SetMode(Mode: TIOMode);
begin
  // Check if SDL2 is required but not available
  if (Mode <> ioTerminal) and not IsSDL2Available then
  begin
    raise EIOManagerError.CreateFmt(
      'I/O mode "%s" requires SDL2, but SDL2 is not available. ' +
      'Falling back to terminal mode.',
      [ModeToString(Mode)]);
  end;

  FMode := Mode;

  // Set default console size based on mode
  case Mode of
    ioTerminal:
      begin
        FWidth := 80;
        FHeight := 25;
      end;
    ioRetroText, ioRetroGfx:
      begin
        FWidth := 40;
        FHeight := 25;
      end;
    ioModernText, ioModernGfx:
      begin
        FWidth := 80;
        FHeight := 50;
      end;
  end;

  // Clear cached devices when mode changes
  FOutputDevice := nil;
  FInputDevice := nil;
end;

function TIOManager.GetMode: TIOMode;
begin
  Result := FMode;
end;

procedure TIOManager.SetWindowTitle(const Title: string);
begin
  FTitle := Title;
end;

procedure TIOManager.SetConsoleSize(Width, Height: Integer);
begin
  FWidth := Width;
  FHeight := Height;
end;

function TIOManager.CreateOutputDevice: IOutputDevice;
begin
  case FMode of
    ioTerminal:
      begin
        Result := TTerminalController.Create;
        Result.Initialize(FTitle, FWidth, FHeight);
      end;

    ioRetroText, ioRetroGfx, ioModernText, ioModernGfx:
      begin
        {$IFDEF HAS_SDL2}
        // Create SDL2 device using the factory
        Result := TOutputDeviceFactory.CreateOutputDevice(otSDL2);
        Result.Initialize(FTitle, FWidth, FHeight);
        {$ELSE}
        // SDL2 not available, fall back to terminal
        Result := TTerminalController.Create;
        Result.Initialize(FTitle, FWidth, FHeight);
        {$ENDIF}
      end;

    else
      raise EIOManagerError.CreateFmt('Unknown I/O mode: %d', [Ord(FMode)]);
  end;
end;

function TIOManager.CreateInputDevice: IInputDevice;
begin
  case FMode of
    ioTerminal:
      Result := TTerminalInput.Create;

    ioRetroText, ioRetroGfx, ioModernText, ioModernGfx:
      begin
        {$IFDEF HAS_SDL2}
        Result := TOutputDeviceFactory.CreateInputDevice(otSDL2);
        {$ELSE}
        // SDL2 not available, fall back to terminal
        Result := TTerminalInput.Create;
        {$ENDIF}
      end;

    else
      raise EIOManagerError.CreateFmt('Unknown I/O mode: %d', [Ord(FMode)]);
  end;
end;

function TIOManager.GetOutputDevice: IOutputDevice;
begin
  if FOutputDevice = nil then
    FOutputDevice := CreateOutputDevice;
  Result := FOutputDevice;
end;

function TIOManager.GetInputDevice: IInputDevice;
begin
  if FInputDevice = nil then
    FInputDevice := CreateInputDevice;
  Result := FInputDevice;
end;

function TIOManager.ModeToString(Mode: TIOMode): string;
begin
  case Mode of
    ioTerminal:    Result := 'terminal';
    ioRetroText:   Result := 'retro-text';
    ioRetroGfx:    Result := 'retro-gfx';
    ioModernText:  Result := 'modern-text';
    ioModernGfx:   Result := 'modern-gfx';
    else           Result := 'unknown';
  end;
end;

function TIOManager.StringToMode(const S: string): TIOMode;
var
  Lower: string;
begin
  Lower := LowerCase(Trim(S));

  if (Lower = 'terminal') or (Lower = 'term') or (Lower = 'console') then
    Result := ioTerminal
  else if (Lower = 'retro-text') or (Lower = 'retro') or (Lower = 'c64') then
    Result := ioRetroText
  else if (Lower = 'retro-gfx') or (Lower = 'retro-graphics') then
    Result := ioRetroGfx
  else if (Lower = 'modern-text') or (Lower = 'modern') or (Lower = '80col') then
    Result := ioModernText
  else if (Lower = 'modern-gfx') or (Lower = 'modern-graphics') or (Lower = 'gfx') then
    Result := ioModernGfx
  else
    raise EIOManagerError.CreateFmt(
      'Unknown I/O mode: "%s". Valid modes: terminal, retro-text, retro-gfx, modern-text, modern-gfx',
      [S]);
end;

end.
