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
unit SedaiEvents;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, sdl2, SedaiExecutorTypes;

type
  TInterruptQueue = specialize TFPGList<TInterruptEvent>;
  TInterruptCallback = procedure(IntType: TInterruptType; Data: Integer) of object;

  TSedaiEventManager = class
  private
    FInterruptQueue: TInterruptQueue;
    FQueueLock: TRTLCriticalSection;
    FEventThread: TThread;
    FVBlankThread: TThread;
    FRunning: Boolean;
    FVBlankEnabled: Boolean;
    FVBlankInterval: Cardinal;
    FLastKeyPressed: Integer;
    FInterruptCallback: TInterruptCallback;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure ProcessSDLEvents;
    function PollInterrupt(out IntType: TInterruptType; out Data: Integer): Boolean;
    procedure QueueInterrupt(IntType: TInterruptType; Data: Integer);

    procedure EnableVBlankInterrupt(Enabled: Boolean; IntervalMs: Cardinal = 16);
    procedure SetInterruptCallback(Callback: TInterruptCallback);

    function GetLastKey: Integer;
    procedure ClearLastKey;

    property Running: Boolean read FRunning;
    property VBlankEnabled: Boolean read FVBlankEnabled;
  end;

  TVBlankThread = class(TThread)
  private
    FEventManager: TSedaiEventManager;
    FInterval: Cardinal;
    FEnabled: Boolean;

  protected
    procedure Execute; override;

  public
    constructor Create(EventManager: TSedaiEventManager; IntervalMs: Cardinal);
    procedure SetEnabled(Enabled: Boolean);
    procedure SetInterval(IntervalMs: Cardinal);
  end;

  TEventThread = class(TThread)
  private
    FEventManager: TSedaiEventManager;

  protected
    procedure Execute; override;

  public
    constructor Create(EventManager: TSedaiEventManager);
  end;

implementation

{ TSedaiEventManager }

constructor TSedaiEventManager.Create;
begin
  inherited Create;
  FInterruptQueue := TInterruptQueue.Create;
  InitCriticalSection(FQueueLock);
  FEventThread := nil;
  FVBlankThread := nil;
  FRunning := False;
  FVBlankEnabled := False;
  FVBlankInterval := 16;
  FLastKeyPressed := 0;
  FInterruptCallback := nil;
end;

destructor TSedaiEventManager.Destroy;
begin
  Stop;
  DoneCriticalSection(FQueueLock);
  FInterruptQueue.Free;
  inherited Destroy;
end;

procedure TSedaiEventManager.Start;
begin
  if FRunning then
    Exit;

  FRunning := True;

  FEventThread := TEventThread.Create(Self);
  FEventThread.Start;

  if FVBlankEnabled then
  begin
    FVBlankThread := TVBlankThread.Create(Self, FVBlankInterval);
    FVBlankThread.Start;
  end;
end;

procedure TSedaiEventManager.Stop;
begin
  if not FRunning then
    Exit;

  FRunning := False;

  if FVBlankThread <> nil then
  begin
    TVBlankThread(FVBlankThread).SetEnabled(False);
    FVBlankThread.Terminate;
    FVBlankThread.WaitFor;
    FVBlankThread.Free;
    FVBlankThread := nil;
  end;

  if FEventThread <> nil then
  begin
    FEventThread.Terminate;
    FEventThread.WaitFor;
    FEventThread.Free;
    FEventThread := nil;
  end;
end;

procedure TSedaiEventManager.ProcessSDLEvents;
var
  Event: TSDL_Event;
begin
  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
        FRunning := False;

      SDL_KEYDOWN:
      begin
        FLastKeyPressed := Event.key.keysym.sym;
        QueueInterrupt(itKeyboard, Event.key.keysym.sym);
      end;

      SDL_KEYUP:
        QueueInterrupt(itKeyboard, -Event.key.keysym.sym);
    end;
  end;
end;

function TSedaiEventManager.PollInterrupt(out IntType: TInterruptType; out Data: Integer): Boolean;
var
  Interrupt: TInterruptEvent;
begin
  Result := False;

  EnterCriticalSection(FQueueLock);
  try
    if FInterruptQueue.Count > 0 then
    begin
      Interrupt := FInterruptQueue[0];
      FInterruptQueue.Delete(0);
      IntType := Interrupt.IntType;
      Data := Interrupt.Data;
      Result := True;
    end;
  finally
    LeaveCriticalSection(FQueueLock);
  end;
end;

procedure TSedaiEventManager.QueueInterrupt(IntType: TInterruptType; Data: Integer);
var
  Interrupt: TInterruptEvent;
begin
  Interrupt.IntType := IntType;
  Interrupt.Data := Data;
  Interrupt.Timestamp := GetTickCount64;

  EnterCriticalSection(FQueueLock);
  try
    FInterruptQueue.Add(Interrupt);
  finally
    LeaveCriticalSection(FQueueLock);
  end;

  if Assigned(FInterruptCallback) then
    FInterruptCallback(IntType, Data);
end;

procedure TSedaiEventManager.EnableVBlankInterrupt(Enabled: Boolean; IntervalMs: Cardinal);
begin
  FVBlankEnabled := Enabled;
  FVBlankInterval := IntervalMs;

  if FRunning then
  begin
    if FVBlankThread <> nil then
    begin
      TVBlankThread(FVBlankThread).SetEnabled(False);
      FVBlankThread.Terminate;
      FVBlankThread.WaitFor;
      FVBlankThread.Free;
      FVBlankThread := nil;
    end;

    if Enabled then
    begin
      FVBlankThread := TVBlankThread.Create(Self, IntervalMs);
      FVBlankThread.Start;
    end;
  end;
end;

procedure TSedaiEventManager.SetInterruptCallback(Callback: TInterruptCallback);
begin
  FInterruptCallback := Callback;
end;

function TSedaiEventManager.GetLastKey: Integer;
begin
  Result := FLastKeyPressed;
end;

procedure TSedaiEventManager.ClearLastKey;
begin
  FLastKeyPressed := 0;
end;

{ TVBlankThread }

constructor TVBlankThread.Create(EventManager: TSedaiEventManager; IntervalMs: Cardinal);
begin
  inherited Create(False);
  FEventManager := EventManager;
  FInterval := IntervalMs;
  FEnabled := True;
  FreeOnTerminate := False;
end;

procedure TVBlankThread.SetEnabled(Enabled: Boolean);
begin
  FEnabled := Enabled;
end;

procedure TVBlankThread.SetInterval(IntervalMs: Cardinal);
begin
  FInterval := IntervalMs;
end;

procedure TVBlankThread.Execute;
begin
  while not Terminated and FEventManager.Running do
  begin
    if FEnabled then
      FEventManager.QueueInterrupt(itVBlank, 0);

    Sleep(FInterval);
  end;
end;

{ TEventThread }

constructor TEventThread.Create(EventManager: TSedaiEventManager);
begin
  inherited Create(False);
  FEventManager := EventManager;
  FreeOnTerminate := False;
end;

procedure TEventThread.Execute;
begin
  while not Terminated and FEventManager.Running do
  begin
    FEventManager.ProcessSDLEvents;
    Sleep(1);
  end;
end;

end.
