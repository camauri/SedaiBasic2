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
  SysUtils, Math, SDL2, SedaiSDL2Dyn, SedaiGraphicsBackend, SedaiGraphicsMemory, SedaiInputState;

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
    procedure HandleEvent(const Event: TSDL_Event);
  public
    constructor Create(ABackend: TSoftwareGraphicsBackend; const Title: string);
    destructor Destroy; override;
    function Pump: Boolean;     // process events + present one frame; True if a close/quit was requested
    function PollEvents: Boolean;   // drain the SDL queue ONLY - no texture upload, no present
    procedure ReportPumpCalls;
    procedure WaitClose;        // keep presenting at ~60 fps until the window is closed
    property Closed: Boolean read FClosed;
  end;
{$ENDIF}

implementation

var
  GPumpCalls: Int64 = 0;
  GPumpNs: Int64 = 0;
  GPumpT0: Int64 = 0;
  GPumpT1: Int64 = 0;

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
  Diag: Boolean;
begin
  X := -1; Y := -1; Wheel := 0; Buttons := 0;
  Result := False;
  // MOUSE_DIAG=1 names WHICH gate answered "no mouse". A GETMOUSE that reports -1 has four
  // different reasons to do so - no window, the pointer over somebody else's window, the pointer
  // outside our rectangle - and from BASIC they are the same answer. Verifying the presenter's
  // mouse without this meant guessing which of them had fired.
  Diag := GetEnvironmentVariable('MOUSE_DIAG') = '1';
  SDL_PumpEvents;
  Focus := SDL_GetMouseFocus();
  if Focus = nil then
  begin
    if Diag then WriteLn(ErrOutput, '[mouse] SDL_GetMouseFocus = nil (pointer over no SDL window of ours)');
    Exit;
  end;
  if (GActiveWindow <> nil) and (Focus <> GActiveWindow) then
  begin
    if Diag then WriteLn(ErrOutput, '[mouse] focus is another window of this process');
    Exit;
  end;
  st := SDL_GetMouseState(@sx, @sy);
  w := 0; h := 0;
  SDL_GetWindowSize(Focus, @w, @h);
  if Diag then WriteLn(ErrOutput, Format('[mouse] sx=%d sy=%d window=%dx%d buttons=%.2x', [sx, sy, w, h, st]));
  if (sx < 0) or (sy < 0) or (sx >= w) or (sy >= h) then Exit;   // pointer off the window
  X := sx; Y := sy;
  // The wheel is a COUNTER, not a position: SDL delivers it as discrete events, so an event watch
  // accumulates the notches and GETMOUSE reports the running total - which is what FreeBASIC's
  // wheel field is. Reading SDL_GetMouseState alone can never see it.
  Wheel := SedaiMouseWheelTotal;
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
    if Id >= SDL_NumJoysticks() then Exit;
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
  // Runtime SDL2 binding (see SedaiSDL2Dyn): --window explicitly asks for a window, so a
  // missing SDL2 library is a hard error here, not a polite degrade.
  if not EnsureSDL2Bound then
    raise Exception.Create('--window requires ' + SDL_LibName + ', which was not found');
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
  SedaiInstallMouseWheelWatch;           // ...and its wheel field, which only an event can fill
  GSetMouseProvider := @WindowSetMouse;  // SETMOUSE warps / toggles the cursor
  GGetJoystickProvider := @WindowGetJoystick;  // GETJOYSTICK/STICK/STRIG read SDL gaming devices
end;

destructor TWindowPresenter.Destroy;
var
  i: Integer;
begin
  GKeyDownProvider := nil;
  GGetMouseProvider := nil;
  SedaiRemoveMouseWheelWatch;
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
  // ⚠️ ARGB, not ABGR. RGB(r,g,b) lowers to (A shl 24) or (R shl 16) or (G shl 8) or B, and a
  // UInt32 in that layout sits in memory little-endian as B,G,R,A - which is exactly what SDL
  // calls ARGB8888. Asking for ABGR8888 swaps red and blue, and the failure is quiet and
  // plausible: a light blue sky comes out light brown and every colour is merely "a bit off",
  // which reads as a palette choice rather than as a bug.
  FTexture := SDL_CreateTexture(FRenderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, W, H);
  FTexW := W; FTexH := H;
  if Assigned(FWindow) then SDL_SetWindowSize(FWindow, W, H);
end;

procedure TWindowPresenter.HandleEvent(const Event: TSDL_Event);
// ⛔ ONE reader for the SDL queue, called by BOTH pumps. There were two copies of this loop - the
// cheap PollEvents on the instruction counter and the full Pump at the frame boundary - and a rule
// added to one of them is a rule the other does not have.
// ⇒ The wheel deliberately does NOT live here even though it is an event: it is counted by the
// watch in SedaiSDL2Dyn, which SDL calls whoever pumps, so the console's fifteen other drain loops
// get it for free (see SedaiInstallMouseWheelWatch).
begin
  if Event.type_ = SDL_QUITEV then FClosed := True
  else if (Event.type_ = SDL_WINDOWEVENT) and (Event.window.event = SDL_WINDOWEVENT_CLOSE) then
    FClosed := True;
end;

function TWindowPresenter.PollEvents: Boolean;
// ⛔ THE CHEAP HALF, AND THE ONE THE RUN LOOP MUST CALL. The VM polls for events every 10 000
// instructions so a window stays responsive while a program computes. Presenting on that schedule is
// what made `sb --window` eight times slower than headless: a frame of this demo is ~1.5 M
// instructions, so the callback fired 158 TIMES PER FRAME and each one uploaded a megabyte and
// presented it. Draining the queue costs nothing; showing the picture belongs at the frame boundary,
// which SCREENUNLOCK now provides.
var
  Event: TSDL_Event;
begin
  while SDL_PollEvent(@Event) <> 0 do
    HandleEvent(Event);
  Result := FClosed;
end;

function TWindowPresenter.Pump: Boolean;
var
  Event: TSDL_Event;
  Mem: TGraphicsMemory;
  W, H, Y, Pitch: Integer;
  Pixels: Pointer;
  Src: PByte;
begin
  Inc(GPumpCalls);
  // SB_NO_PUMP=1: count the call but do nothing. An A/B knob on ONE binary, which is how the windowed
  // frame time was split into its three parts - the C hot loop being off (29%), the mere presence of
  // the window (52%) and the present itself (19%). Two separately linked binaries could not have
  // answered that: this project has measured 14% from code alignment alone.
  if GetEnvironmentVariable('SB_NO_PUMP') = '1' then begin Result := FClosed; Exit; end;
  GPumpT0 := GetTickCount64;
  // Drain the SDL event queue (quit / window close -> request abort, wheel notches accumulate).
  while SDL_PollEvent(@Event) <> 0 do
    HandleEvent(Event);

  // Mirror the software framebuffer (ARGB) into the texture and present it.
  if Assigned(FRenderer) and Assigned(FBackend) then
  begin
    Mem := FBackend.ScreenMemory;
    if Assigned(Mem) and Assigned(Mem.GraphicsBuffer) then
    begin
      W := Mem.State.Width; H := Mem.State.Height;
      EnsureTexture(W, H);
      // ⛔ SDL_UpdateTexture, NOT SDL_LockTexture. Locking a STREAMING texture asks the driver for a
      // writable mapping, and on this stack (i915/DRM) that means FRESH PAGES EVERY FRAME: the kernel
      // has to zero them and flush them out of the CPU cache before the GPU may read them.
      // 📊 Measured 22 Aug 2026 on a 500x500 demo: 345 extra page faults PER FRAME (16 052 against
      // 2 254 for the same program headless), and `perf` put 25.8% of the whole run in
      // clear_page_erms and 11.4% in drm_clflush_page - 37% of the time spent preparing memory that
      // was thrown away one frame later. The demo ran at 8 fps in a window against 64 headless.
      // UpdateTexture hands the driver our existing buffer and lets it copy: no mapping, no new pages.
      // ⚠️ It needs the source rows to be CONTIGUOUS at a known pitch, which the software backend's
      // framebuffer is (W*4 bytes per row, no padding) - that is why the per-row Move loop this
      // replaces was copying with a DIFFERENT destination pitch and is not needed here.
      if Assigned(FTexture) then
        SDL_UpdateTexture(FTexture, nil, Mem.GraphicsBuffer, W * 4);
    end;
    SDL_SetRenderDrawColor(FRenderer, 0, 0, 0, 255);
    SDL_RenderClear(FRenderer);
    if Assigned(FTexture) then SDL_RenderCopy(FRenderer, FTexture, nil, nil);
    SDL_RenderPresent(FRenderer);
  end;

  GPumpT1 := GetTickCount64;
  Inc(GPumpNs, GPumpT1 - GPumpT0);
  Result := FClosed;
end;

procedure TWindowPresenter.ReportPumpCalls;
// PUMP_DIAG=1: how many times the picture was actually shown. ⭐ This counter is why `sb --window` is
// no longer eight times slower than headless: it read 4 732 for a 30-frame run - 158 presents per
// frame - which no amount of reading the code had suggested, and one present per frame is 61.
begin
  if GetEnvironmentVariable('PUMP_DIAG') = '1' then
    WriteLn(ErrOutput, Format('[pump] presentazioni = %d, tempo totale = %.0f ms (%.2f ms l''una)', [GPumpCalls, GPumpNs*1.0, GPumpNs*1.0/GPumpCalls]));
end;

procedure TWindowPresenter.WaitClose;
begin
  while not Pump do
    SDL_Delay(16);
end;

{$ENDIF}

end.
