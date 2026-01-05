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
unit SedaiOutputFactory;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiOutputInterface,
  SedaiSDL2Input,
  SedaiSDL2GraphicsOutput;

type
  { Factory per creare devices separati - CORRETTO }
  { TOutputDeviceFactory }
  TOutputDeviceFactory = class
  private
    class var FCurrentSDL2Device: TSDL2GraphicsOutputDevice;
    class var FLastSDL2Input: TSDL2InputDevice; // Riferimento per linking
  public
    class function CreateOutputDevice(OutputType: TOutputType): IOutputDevice;
    class function CreateInputDevice(OutputType: TOutputType): IInputDevice;
    class function GetCurrentSDL2Device: TSDL2GraphicsOutputDevice;
    class function GetSDL2InputDevice: TSDL2InputDevice;
  end;

implementation

class function TOutputDeviceFactory.CreateOutputDevice(OutputType: TOutputType): IOutputDevice;
begin
  case OutputType of
    otSDL2:
      begin
        // Create the graphics device that auto-detects capabilities
        FCurrentSDL2Device := TSDL2GraphicsOutputDevice.Create;
        Result := FCurrentSDL2Device;
      end;
    else
      raise Exception.CreateFmt('Only SDL2 output is supported (requested: %d)', [Ord(OutputType)]);
  end;
end;

class function TOutputDeviceFactory.CreateInputDevice(OutputType: TOutputType): IInputDevice;
begin
  case OutputType of
    otSDL2:
      begin
        FLastSDL2Input := TSDL2InputDevice.Create;
        Result := FLastSDL2Input;
      end;
    else
      raise Exception.CreateFmt('Only SDL2 input is supported (requested: %d)', [Ord(OutputType)]);
  end;
end;

class function TOutputDeviceFactory.GetCurrentSDL2Device: TSDL2GraphicsOutputDevice;
begin
  Result := FCurrentSDL2Device;
end;

class function TOutputDeviceFactory.GetSDL2InputDevice: TSDL2InputDevice;
begin
  Result := FLastSDL2Input;
end;

end.
