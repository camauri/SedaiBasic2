{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

{ Real-time interactive input state (MULTIKEY / GETMOUSE / SETMOUSE), decoupled from SDL.

  The VM is SDL-agnostic, but real-time key/mouse state lives in whatever owns the graphics window
  (the sb --window presenter, or sbv's TVideoController). Rather than thread it through IInputDevice and
  every implementer, the VM queries the global provider hooks below. The window owner installs them; when
  none is installed (headless sb), they stay nil and the VM reports "no input" (key up, mouse at -1).

  ATScancodeToSDL maps FreeBASIC MULTIKEY scancodes (PC/AT set-1, as in fbgfx.bi SC_*) to SDL scancodes,
  so the providers can index SDL_GetKeyboardState directly. }
unit SedaiInputState;

{$mode ObjFPC}{$H+}

interface

type
  // True if the key with the given FB (AT set-1) scancode is currently held down.
  TKeyDownFn = function(ATScanCode: Integer): Boolean;
  // Fills the current mouse state (window-relative). Returns False if no mouse / off-window (FB: x=-1).
  TGetMouseFn = function(out X, Y, Wheel, Buttons: Integer): Boolean;
  // Move the mouse cursor / set its visibility. Each parameter is -1 for "no change" (FB SetMouse
  // semantics); Visibility is 0 = hide, 1 = show.
  TSetMouseFn = procedure(X, Y, Visibility: Integer);

var
  GKeyDownProvider: TKeyDownFn = nil;
  GGetMouseProvider: TGetMouseFn = nil;
  GSetMouseProvider: TSetMouseFn = nil;

// FB MULTIKEY scancode (PC/AT set 1) -> SDL_Scancode. Returns 0 (SDL_SCANCODE_UNKNOWN) if unmapped.
function ATScancodeToSDL(AT: Integer): Integer;

implementation

function ATScancodeToSDL(AT: Integer): Integer;
begin
  case AT of
    $01: Result := 41;   // ESC
    $02: Result := 30;   // 1
    $03: Result := 31;   // 2
    $04: Result := 32;   // 3
    $05: Result := 33;   // 4
    $06: Result := 34;   // 5
    $07: Result := 35;   // 6
    $08: Result := 36;   // 7
    $09: Result := 37;   // 8
    $0A: Result := 38;   // 9
    $0B: Result := 39;   // 0
    $0C: Result := 45;   // -
    $0D: Result := 46;   // =
    $0E: Result := 42;   // BACKSPACE
    $0F: Result := 43;   // TAB
    $10: Result := 20;   // Q
    $11: Result := 26;   // W
    $12: Result := 8;    // E
    $13: Result := 21;   // R
    $14: Result := 23;   // T
    $15: Result := 28;   // Y
    $16: Result := 24;   // U
    $17: Result := 12;   // I
    $18: Result := 18;   // O
    $19: Result := 19;   // P
    $1A: Result := 47;   // [
    $1B: Result := 48;   // ]
    $1C: Result := 40;   // ENTER
    $1D: Result := 224;  // LCTRL
    $1E: Result := 4;    // A
    $1F: Result := 22;   // S
    $20: Result := 7;    // D
    $21: Result := 9;    // F
    $22: Result := 10;   // G
    $23: Result := 11;   // H
    $24: Result := 13;   // J
    $25: Result := 14;   // K
    $26: Result := 15;   // L
    $27: Result := 51;   // ;
    $28: Result := 52;   // '
    $29: Result := 53;   // `
    $2A: Result := 225;  // LSHIFT
    $2B: Result := 49;   // backslash
    $2C: Result := 29;   // Z
    $2D: Result := 27;   // X
    $2E: Result := 6;    // C
    $2F: Result := 25;   // V
    $30: Result := 5;    // B
    $31: Result := 17;   // N
    $32: Result := 16;   // M
    $33: Result := 54;   // ,
    $34: Result := 55;   // .
    $35: Result := 56;   // /
    $36: Result := 229;  // RSHIFT
    $38: Result := 226;  // LALT
    $39: Result := 44;   // SPACE
    $3A: Result := 57;   // CAPSLOCK
    $3B: Result := 58;   // F1
    $3C: Result := 59;   // F2
    $3D: Result := 60;   // F3
    $3E: Result := 61;   // F4
    $3F: Result := 62;   // F5
    $40: Result := 63;   // F6
    $41: Result := 64;   // F7
    $42: Result := 65;   // F8
    $43: Result := 66;   // F9
    $44: Result := 67;   // F10
    $47: Result := 74;   // HOME
    $48: Result := 82;   // UP
    $49: Result := 75;   // PAGEUP
    $4B: Result := 80;   // LEFT
    $4D: Result := 79;   // RIGHT
    $4F: Result := 77;   // END
    $50: Result := 81;   // DOWN
    $51: Result := 78;   // PAGEDOWN
    $52: Result := 73;   // INSERT
    $53: Result := 76;   // DELETE
  else
    Result := 0;         // SDL_SCANCODE_UNKNOWN
  end;
end;

end.
