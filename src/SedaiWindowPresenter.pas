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

{ Lightweight SDL2 window presenter for the CLI VM (`sb --window`).

  `sb` stays headless by default (the regression target). When built WITH_WINDOW and run with --window,
  this presenter mirrors the software graphics backend's CPU framebuffer (the source of truth) into an
  SDL2 window: it blits the framebuffer to a streaming texture each frame and pumps the SDL event queue.
  It is wired as the VM's EventPollCallback, so drawing appears progressively and closing the window
  aborts the program. NOT compiled unless WITH_WINDOW is defined (no SDL2 dependency for the headless
  build). Keyboard/mouse input wiring for MULTIKEY/GETMOUSE builds on the same event pump (next step). }
unit SedaiWindowPresenter;

{$mode ObjFPC}{$H+}
{$codepage UTF8}

interface

{$IFDEF WITH_WINDOW}
uses
  SysUtils, Math, SDL2, SedaiGraphicsBackend, SedaiGraphicsMemory, SedaiInputState;

type
  TWindowPresenter = class
  private
    FBackend: TSoftwareGraphicsBackend;
    FWindow: PSDL_Window;
    FRenderer: PSDL_Renderer;
    FTexture: PSDL_Texture;
    FTexW, FTexH: Integer;
    FClosed: Boolean;
    procedure EnsureTexture(W, H: Integer);
  public
    constructor Create(ABackend: TSoftwareGraphicsBackend; const Title: string);
    destructor Destroy; override;
    function Pump: Boolean;     // process events + present one frame; True if a close/quit was requested
    procedure WaitClose;        // keep presenting at ~60 fps until the window is closed
    property Closed: Boolean read FClosed;
  end;
{$ENDIF}

implementation

{$IFDEF WITH_WINDOW}

// Real-time key state for MULTIKEY (installed as GKeyDownProvider while the window is open).
function WindowKeyDown(ATScanCode: Integer): Boolean;
var
  NumKeys, SdlSc: Integer;
  State: PUInt8;
begin
  Result := False;
  SDL_PumpEvents;
  State := SDL_GetKeyboardState(@NumKeys);
  if State = nil then Exit;
  SdlSc := ATScancodeToSDL(ATScanCode);
  Result := (SdlSc > 0) and (SdlSc < NumKeys) and ((State + SdlSc)^ <> 0);
end;

constructor TWindowPresenter.Create(ABackend: TSoftwareGraphicsBackend; const Title: string);
begin
  inherited Create;
  FBackend := ABackend;
  FClosed := False;
  FTexW := 0; FTexH := 0;
  FTexture := nil;
  // FPC leaves FP exceptions UNMASKED; SDL2 and GPU drivers do internal FP that produces inf/NaN
  // (e.g. divide-by-zero) and expect them masked — otherwise SDL_CreateRenderer raises EZeroDivide.
  // Safe here: the VM detects BASIC division-by-zero with explicit value checks, not the FPU trap.
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  if SDL_WasInit(SDL_INIT_VIDEO) = 0 then
    SDL_InitSubSystem(SDL_INIT_VIDEO);
  FWindow := SDL_CreateWindow(PChar(Title), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                              640, 480, SDL_WINDOW_SHOWN);
  if Assigned(FWindow) then
    FRenderer := SDL_CreateRenderer(FWindow, -1, SDL_RENDERER_ACCELERATED)
  else
    FRenderer := nil;
  GKeyDownProvider := @WindowKeyDown;   // MULTIKEY now reads the live SDL keyboard state
end;

destructor TWindowPresenter.Destroy;
begin
  GKeyDownProvider := nil;
  if Assigned(FTexture) then SDL_DestroyTexture(FTexture);
  if Assigned(FRenderer) then SDL_DestroyRenderer(FRenderer);
  if Assigned(FWindow) then SDL_DestroyWindow(FWindow);
  inherited Destroy;
end;

procedure TWindowPresenter.EnsureTexture(W, H: Integer);
begin
  if (W <= 0) or (H <= 0) or not Assigned(FRenderer) then Exit;
  if Assigned(FTexture) and (W = FTexW) and (H = FTexH) then Exit;
  if Assigned(FTexture) then SDL_DestroyTexture(FTexture);
  FTexture := SDL_CreateTexture(FRenderer, SDL_PIXELFORMAT_ABGR8888, SDL_TEXTUREACCESS_STREAMING, W, H);
  FTexW := W; FTexH := H;
  if Assigned(FWindow) then SDL_SetWindowSize(FWindow, W, H);
end;

function TWindowPresenter.Pump: Boolean;
var
  Event: TSDL_Event;
  Mem: TGraphicsMemory;
  W, H, Y, Pitch: Integer;
  Pixels: Pointer;
  Src: PByte;
begin
  // Drain the SDL event queue (quit / window close -> request abort).
  while SDL_PollEvent(@Event) <> 0 do
  begin
    if Event.type_ = SDL_QUITEV then FClosed := True
    else if (Event.type_ = SDL_WINDOWEVENT) and (Event.window.event = SDL_WINDOWEVENT_CLOSE) then
      FClosed := True;
  end;

  // Mirror the software framebuffer (ABGR) into the texture and present it.
  if Assigned(FRenderer) and Assigned(FBackend) then
  begin
    Mem := FBackend.ScreenMemory;
    if Assigned(Mem) and Assigned(Mem.GraphicsBuffer) then
    begin
      W := Mem.State.Width; H := Mem.State.Height;
      EnsureTexture(W, H);
      if Assigned(FTexture) and (SDL_LockTexture(FTexture, nil, @Pixels, @Pitch) = 0) then
      begin
        Src := Mem.GraphicsBuffer;
        for Y := 0 to H - 1 do
          Move((Src + Y * W * 4)^, (PByte(Pixels) + Y * Pitch)^, W * 4);
        SDL_UnlockTexture(FTexture);
      end;
    end;
    SDL_SetRenderDrawColor(FRenderer, 0, 0, 0, 255);
    SDL_RenderClear(FRenderer);
    if Assigned(FTexture) then SDL_RenderCopy(FRenderer, FTexture, nil, nil);
    SDL_RenderPresent(FRenderer);
  end;

  Result := FClosed;
end;

procedure TWindowPresenter.WaitClose;
begin
  while not Pump do
    SDL_Delay(16);
end;

{$ENDIF}

end.
