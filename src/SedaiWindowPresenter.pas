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

var
  GActiveWindow: PSDL_Window = nil;   // the presenter's window while open (for SetMouse warp / bounds)
  GJoy: array[0..15] of PSDL_Joystick;   // lazily-opened gaming devices (GETJOYSTICK/STICK/STRIG)

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

// GETMOUSE: window-relative position + button bitmask (installed as GGetMouseProvider while open).
// Returns False (FB: all -1, status 1) when the pointer is not over our window. Wheel is 0 (untracked v1).
function WindowGetMouse(out X, Y, Wheel, Buttons: Integer): Boolean;
var
  sx, sy, w, h: Integer;
  st: UInt32;
  Focus: PSDL_Window;
begin
  X := -1; Y := -1; Wheel := 0; Buttons := 0;
  Result := False;
  SDL_PumpEvents;
  Focus := SDL_GetMouseFocus;
  if Focus = nil then Exit;
  if (GActiveWindow <> nil) and (Focus <> GActiveWindow) then Exit;
  st := SDL_GetMouseState(@sx, @sy);
  w := 0; h := 0;
  SDL_GetWindowSize(Focus, @w, @h);
  if (sx < 0) or (sy < 0) or (sx >= w) or (sy >= h) then Exit;   // pointer off the window
  X := sx; Y := sy;
  // SDL mask (L=1,M=2,R=4) -> FB bitmask (bit0=left, bit1=right, bit2=middle).
  if (st and SDL_BUTTON_LMASK) <> 0 then Buttons := Buttons or 1;
  if (st and SDL_BUTTON_RMASK) <> 0 then Buttons := Buttons or 2;
  if (st and SDL_BUTTON_MMASK) <> 0 then Buttons := Buttons or 4;
  Result := True;
end;

// SETMOUSE: warp the pointer and/or toggle cursor visibility (-1 = no change on each field).
procedure WindowSetMouse(X, Y, Visibility: Integer);
begin
  if (GActiveWindow <> nil) and (X >= 0) and (Y >= 0) then
    SDL_WarpMouseInWindow(GActiveWindow, X, Y);
  if Visibility = 0 then SDL_ShowCursor(SDL_DISABLE)
  else if Visibility = 1 then SDL_ShowCursor(SDL_ENABLE);
end;

// GETJOYSTICK / STICK / STRIG: read gaming device `Id` (lazily opened). Fills the button bitmask and up
// to MaxAxes axis values (SDL int16 -32768..32767 normalised to -1..1; -1000 if the axis is absent).
// Returns False if the device is not present.
function WindowGetJoystick(Id: Integer; out Buttons: Integer; Axes: PSingle; MaxAxes: Integer): Boolean;
var
  i, n: Integer;
begin
  Buttons := 0;
  for i := 0 to MaxAxes - 1 do Axes[i] := -1000.0;
  Result := False;
  if (Id < 0) or (Id > 15) then Exit;
  if SDL_WasInit(SDL_INIT_JOYSTICK) = 0 then SDL_InitSubSystem(SDL_INIT_JOYSTICK);
  if GJoy[Id] = nil then
  begin
    if Id >= SDL_NumJoysticks then Exit;
    GJoy[Id] := SDL_JoystickOpen(Id);
    if GJoy[Id] = nil then Exit;
  end;
  SDL_JoystickUpdate;
  n := SDL_JoystickNumAxes(GJoy[Id]);
  for i := 0 to MaxAxes - 1 do
    if i < n then Axes[i] := SDL_JoystickGetAxis(GJoy[Id], i) / 32767.0
    else Axes[i] := -1000.0;
  n := SDL_JoystickNumButtons(GJoy[Id]);
  for i := 0 to n - 1 do
    if (i < 32) and (SDL_JoystickGetButton(GJoy[Id], i) <> 0) then Buttons := Buttons or (1 shl i);
  Result := True;
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
  GActiveWindow := FWindow;
  GKeyDownProvider := @WindowKeyDown;    // MULTIKEY reads the live SDL keyboard state
  GGetMouseProvider := @WindowGetMouse;  // GETMOUSE reads the live SDL mouse state
  GSetMouseProvider := @WindowSetMouse;  // SETMOUSE warps / toggles the cursor
  GGetJoystickProvider := @WindowGetJoystick;  // GETJOYSTICK/STICK/STRIG read SDL gaming devices
end;

destructor TWindowPresenter.Destroy;
var
  i: Integer;
begin
  GKeyDownProvider := nil;
  GGetMouseProvider := nil;
  GSetMouseProvider := nil;
  GGetJoystickProvider := nil;
  GActiveWindow := nil;
  for i := 0 to High(GJoy) do
    if GJoy[i] <> nil then begin SDL_JoystickClose(GJoy[i]); GJoy[i] := nil; end;
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
