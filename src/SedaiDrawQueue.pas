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
unit SedaiDrawQueue;

{$mode ObjFPC}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SyncObjs, SedaiBytecodeTypes;

{ Render command queue — M5.3 (multithreading groundwork, SEDAIBASIC_EVOLUTION.md S16.3).

  The one hard rule once OS threads land (M5.2): ONLY the render-owner thread may touch the
  SDL device. So a worker thread does not execute graphics/sprite opcodes directly — it
  enqueues them here, fully resolved (the operand registers it reads are snapshotted at
  enqueue time, since by drain time the worker's context has moved on). The owner thread
  drains the queue at its present cadence ([[graphics-present-cadence]]) and replays each
  command on the real device.

  In single-threaded operation (today, and whenever no worker has been spawned) the queue is
  never used: the VM gates every enqueue/drain behind its FHasWorkers flag, which is False
  until M5.2, so graphics opcodes execute inline exactly as before. This type therefore adds
  no behaviour and no overhead to the current single-threaded path — it is the seam M5.2
  fills in. }

type
  TDrawCommandKind = (dckGraphics, dckSprite);

  { One deferred graphics/sprite opcode plus the resolved register snapshot it needs.
    The snapshot copies the producer thread's register banks at enqueue time so the owner
    thread can replay the opcode handler (which reads FCtx.IntRegs[...] etc.) faithfully,
    without per-opcode argument marshaling. }
  TDrawCommand = record
    Kind: TDrawCommandKind;
    Instr: TBytecodeInstruction;
    IntRegs: array of Int64;
    FloatRegs: array of Double;
    StringRegs: array of string;
  end;

  { Thread-safe multi-producer / single-consumer queue. Producers (worker threads) Enqueue;
    the single render-owner thread DequeueAll once per frame. }
  TDrawCommandQueue = class
  private
    FLock: TCriticalSection;
    FItems: array of TDrawCommand;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Cmd: TDrawCommand);
    // Atomically moves all pending commands into Out (Out length = number drained) and clears
    // the queue. Returns the count. Called only by the render-owner thread.
    function DequeueAll(out Items: array of TDrawCommand): Integer;
    function IsEmpty: Boolean;
  end;

implementation

constructor TDrawCommandQueue.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCount := 0;
end;

destructor TDrawCommandQueue.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TDrawCommandQueue.Enqueue(const Cmd: TDrawCommand);
begin
  FLock.Acquire;
  try
    if FCount >= Length(FItems) then
    begin
      if Length(FItems) = 0 then
        SetLength(FItems, 64)
      else
        SetLength(FItems, Length(FItems) * 2);
    end;
    FItems[FCount] := Cmd;
    Inc(FCount);
  finally
    FLock.Release;
  end;
end;

function TDrawCommandQueue.DequeueAll(out Items: array of TDrawCommand): Integer;
var
  i: Integer;
begin
  FLock.Acquire;
  try
    Result := FCount;
    if Result > Length(Items) then
      Result := Length(Items);
    for i := 0 to Result - 1 do
      Items[i] := FItems[i];
    FCount := 0;
  finally
    FLock.Release;
  end;
end;

function TDrawCommandQueue.IsEmpty: Boolean;
begin
  FLock.Acquire;
  try
    Result := FCount = 0;
  finally
    FLock.Release;
  end;
end;

end.
