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
{ ============================================================================
  Unit: SedaiSDL2Dyn (runtime-loaded SDL2/SDL2_ttf bindings)

  Purpose: let the units SHARED between the headless CLI (sb) and the SDL
  frontends (sbv, sb --window) call SDL without creating a STATIC import of
  SDL2.dll. A static import makes the Windows loader map SDL2.dll - and its
  own dependency tree - on EVERY sb launch, measured at several Mcycles of
  the headless startup, for a library headless runs never call.

  How it works: this unit declares procedural VARIABLES with the very same
  names and signatures as the static externals in sdl2.pas / sdl2_ttf.pas.
  A unit that lists SedaiSDL2Dyn AFTER SDL2/SDL2_ttf in its uses clause
  resolves those names to the variables here (later unit wins), so its call
  sites compile unchanged - but reference no import. TYPES and CONSTANTS
  keep coming from sdl2.pas/sdl2_ttf.pas: they cost no import.

  EnsureSDL2Bound loads the libraries (via the bindings' own per-platform
  SDL_LibName/TTF_LibName) and fills every pointer, once. It must be called
  on the few SDL entry bottlenecks (console InitializeSDL, video-mode
  enumeration, joystick access, window presenter creation) BEFORE any
  wrapped call. TTF is optional: if its library is absent the TTF pointers
  stay nil and SDL2DynTTFAvailable reports False (callers degrade politely
  instead of the process failing to start, which is what the static import
  did). In the SDL frontends the DLL is typically already mapped by their
  own static bindings, so binding here is just address lookups.

  ⚠️ If a shared unit starts calling an SDL function that is NOT declared
  below, the name silently resolves to the static external again and the
  import returns: after touching SDL code in shared units, check the import
  table (objdump -p sb.exe | grep DLL) or the startup cycle count.
  ============================================================================ }

unit SedaiSDL2Dyn;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  ctypes, SDL2, SDL2_ttf;

var
  { --- SDL2 core (same names/signatures as the static externals) --- }
  SDL_Init: function(flags: TSDL_Init): cint; cdecl;
  SDL_InitSubSystem: function(flags: TSDL_Init): cint; cdecl;
  SDL_WasInit: function(flags: TSDL_Init): cuint32; cdecl;
  SDL_Quit: procedure; cdecl;
  SDL_GetError: function: PAnsiChar; cdecl;
  SDL_SetHint: function(const name: PAnsiChar; const value: PAnsiChar): TSDL_Bool; cdecl;
  SDL_GetTicks: function: cuint32; cdecl;
  SDL_Delay: procedure(ms: cuint32); cdecl;

  { windows / renderers }
  SDL_CreateWindow: function(const title: PAnsiChar; x: cint; y: cint; w: cint; h: cint; flags: TSDL_WindowFlags): PSDL_Window; cdecl;
  SDL_DestroyWindow: procedure(window: PSDL_Window); cdecl;
  SDL_GetWindowSize: procedure(window: PSDL_Window; w: pcint; h: pcint); cdecl;
  SDL_SetWindowSize: procedure(window: PSDL_Window; w: cint; h: cint); cdecl;
  SDL_CreateRenderer: function(window: PSDL_Window; index: cint32; flags: cuint32): PSDL_Renderer; cdecl;
  SDL_DestroyRenderer: procedure(renderer: PSDL_Renderer); cdecl;
  SDL_GetRendererOutputSize: function(renderer: PSDL_Renderer; w: pcint; h: pcint): cint32; cdecl;
  SDL_RenderClear: function(renderer: PSDL_Renderer): cint32; cdecl;
  SDL_RenderCopy: function(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_Rect; dstrect: PSDL_Rect): cint32; cdecl;
  SDL_RenderDrawLine: function(renderer: PSDL_Renderer; x1: cint32; y1: cint32; x2: cint32; y2: cint32): cint32; cdecl;
  SDL_RenderDrawRect: function(renderer: PSDL_Renderer; rect: PSDL_Rect): cint32; cdecl;
  SDL_RenderFillRect: function(renderer: PSDL_Renderer; rect: PSDL_Rect): cint32; cdecl;
  SDL_RenderPresent: procedure(renderer: PSDL_Renderer); cdecl;
  SDL_RenderGetClipRect: procedure(renderer: PSDL_Renderer; rect: PSDL_Rect); cdecl;
  SDL_RenderSetClipRect: function(renderer: PSDL_Renderer; rect: PSDL_Rect): cint32; cdecl;
  SDL_RenderIsClipEnabled: function(renderer: PSDL_Renderer): TSDL_Bool; cdecl;
  SDL_SetRenderDrawBlendMode: function(renderer: PSDL_Renderer; blendMode: TSDL_BlendMode): cint32; cdecl;
  SDL_SetRenderDrawColor: function(renderer: PSDL_Renderer; r: cuint8; g: cuint8; b: cuint8; a: cuint8): cint32; cdecl;
  SDL_SetRenderTarget: function(renderer: PSDL_Renderer; texture: PSDL_Texture): cint32; cdecl;

  { textures / surfaces }
  SDL_CreateTexture: function(renderer: PSDL_Renderer; format: cuint32; access: cint32; w: cint32; h: cint32): PSDL_Texture; cdecl;
  SDL_CreateTextureFromSurface: function(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture; cdecl;
  SDL_DestroyTexture: procedure(texture: PSDL_Texture); cdecl;
  SDL_LockTexture: function(texture: PSDL_Texture; const rect: PSDL_Rect; pixels: PPointer; pitch: pcint): cint; cdecl;
  SDL_UnlockTexture: procedure(texture: PSDL_Texture); cdecl;
  SDL_UpdateTexture: function(texture: PSDL_Texture; rect: PSDL_Rect; pixels: Pointer; pitch: cint32): cint32; cdecl;
  SDL_SetTextureAlphaMod: function(texture: PSDL_Texture; alpha: cuint8): cint32; cdecl;
  SDL_SetTextureBlendMode: function(texture: PSDL_Texture; blendMode: TSDL_BlendMode): cint32; cdecl;
  SDL_SetTextureColorMod: function(texture: PSDL_Texture; r: cuint8; g: cuint8; b: cuint8): cint32; cdecl;
  SDL_FreeSurface: procedure(surface: PSDL_Surface); cdecl;

  { display modes }
  SDL_GetNumDisplayModes: function(displayIndex: cint): cint; cdecl;
  SDL_GetDisplayMode: function(displayIndex: cint; modeIndex: cint; mode: PSDL_DisplayMode): cint; cdecl;
  SDL_GetDesktopDisplayMode: function(displayIndex: cint; mode: PSDL_DisplayMode): cint; cdecl;
  SDL_GetCurrentDisplayMode: function(displayIndex: cint; mode: PSDL_DisplayMode): cint; cdecl;

  { events / input }
  SDL_PollEvent: function(event: PSDL_Event): cint32; cdecl;
  SDL_PumpEvents: procedure; cdecl;
  SDL_StartTextInput: procedure; cdecl;
  SDL_StopTextInput: procedure; cdecl;
  SDL_GetKeyboardState: function(numkeys: pcint): pcuint8; cdecl;
  SDL_GetModState: function: TSDL_Keymod; cdecl;
  SDL_GetMouseFocus: function: PSDL_Window; cdecl;
  SDL_GetMouseState: function(x: pcint; y: pcint): cuint32; cdecl;
  SDL_WarpMouseInWindow: procedure(window: PSDL_Window; x: cint; y: cint); cdecl;
  SDL_ShowCursor: function(toggle: cint): cint; cdecl;

  { joystick }
  SDL_NumJoysticks: function(): cint; cdecl;
  SDL_JoystickOpen: function(device_index: cint): PSDL_Joystick; cdecl;
  SDL_JoystickUpdate: procedure(); cdecl;
  SDL_JoystickNumAxes: function(joystick: PSDL_Joystick): cint; cdecl;
  SDL_JoystickNumButtons: function(joystick: PSDL_Joystick): cint; cdecl;
  SDL_JoystickGetAxis: function(joystick: PSDL_Joystick; axis: cint): cint16; cdecl;
  SDL_JoystickGetButton: function(joystick: PSDL_Joystick; button: cint): cuint8; cdecl;
  SDL_JoystickClose: procedure(joystick: PSDL_Joystick); cdecl;

  { --- SDL2_ttf (optional: nil when the library is absent) --- }
  TTF_Init: function(): cint; cdecl;
  TTF_Quit: procedure(); cdecl;
  TTF_OpenFont: function(file_: PAnsiChar; ptsize: cint): PTTF_Font; cdecl;
  TTF_CloseFont: procedure(font: PTTF_Font); cdecl;
  TTF_SetFontStyle: procedure(font: PTTF_Font; style: cint); cdecl;
  TTF_SizeUTF8: function(font: PTTF_Font; text: PAnsiChar; w: pcint; h: pcint): cint; cdecl;
  TTF_RenderUTF8_Solid: function(font: PTTF_Font; text: PAnsiChar; fg: TSDL_Color): PSDL_Surface; cdecl;
  TTF_RenderUTF8_Blended: function(font: PTTF_Font; text: PAnsiChar; fg: TSDL_Color): PSDL_Surface; cdecl;

{ Load SDL2 (and, best-effort, SDL2_ttf) and bind every pointer above. Idempotent and cheap
  after the first call. Returns False when the SDL2 library cannot be loaded - headless
  callers treat that as "no device". }
function EnsureSDL2Bound: Boolean;

{ True when SDL2_ttf was found and its pointers are bound. }
function SDL2DynTTFAvailable: Boolean;

implementation

uses
  SysUtils, dynlibs;

var
  GSDL2: TLibHandle = NilHandle;
  GTTF: TLibHandle = NilHandle;
  GTried: Boolean = False;

function EnsureSDL2Bound: Boolean;
begin
  if GTried then Exit(GSDL2 <> NilHandle);
  GTried := True;

  GSDL2 := LoadLibrary(SDL_LibName);
  if GSDL2 = NilHandle then Exit(False);

  Pointer(SDL_Init) := GetProcedureAddress(GSDL2, 'SDL_Init');
  Pointer(SDL_InitSubSystem) := GetProcedureAddress(GSDL2, 'SDL_InitSubSystem');
  Pointer(SDL_WasInit) := GetProcedureAddress(GSDL2, 'SDL_WasInit');
  Pointer(SDL_Quit) := GetProcedureAddress(GSDL2, 'SDL_Quit');
  Pointer(SDL_GetError) := GetProcedureAddress(GSDL2, 'SDL_GetError');
  Pointer(SDL_SetHint) := GetProcedureAddress(GSDL2, 'SDL_SetHint');
  Pointer(SDL_GetTicks) := GetProcedureAddress(GSDL2, 'SDL_GetTicks');
  Pointer(SDL_Delay) := GetProcedureAddress(GSDL2, 'SDL_Delay');
  Pointer(SDL_CreateWindow) := GetProcedureAddress(GSDL2, 'SDL_CreateWindow');
  Pointer(SDL_DestroyWindow) := GetProcedureAddress(GSDL2, 'SDL_DestroyWindow');
  Pointer(SDL_GetWindowSize) := GetProcedureAddress(GSDL2, 'SDL_GetWindowSize');
  Pointer(SDL_SetWindowSize) := GetProcedureAddress(GSDL2, 'SDL_SetWindowSize');
  Pointer(SDL_CreateRenderer) := GetProcedureAddress(GSDL2, 'SDL_CreateRenderer');
  Pointer(SDL_DestroyRenderer) := GetProcedureAddress(GSDL2, 'SDL_DestroyRenderer');
  Pointer(SDL_GetRendererOutputSize) := GetProcedureAddress(GSDL2, 'SDL_GetRendererOutputSize');
  Pointer(SDL_RenderClear) := GetProcedureAddress(GSDL2, 'SDL_RenderClear');
  Pointer(SDL_RenderCopy) := GetProcedureAddress(GSDL2, 'SDL_RenderCopy');
  Pointer(SDL_RenderDrawLine) := GetProcedureAddress(GSDL2, 'SDL_RenderDrawLine');
  Pointer(SDL_RenderDrawRect) := GetProcedureAddress(GSDL2, 'SDL_RenderDrawRect');
  Pointer(SDL_RenderFillRect) := GetProcedureAddress(GSDL2, 'SDL_RenderFillRect');
  Pointer(SDL_RenderPresent) := GetProcedureAddress(GSDL2, 'SDL_RenderPresent');
  Pointer(SDL_RenderGetClipRect) := GetProcedureAddress(GSDL2, 'SDL_RenderGetClipRect');
  Pointer(SDL_RenderSetClipRect) := GetProcedureAddress(GSDL2, 'SDL_RenderSetClipRect');
  Pointer(SDL_RenderIsClipEnabled) := GetProcedureAddress(GSDL2, 'SDL_RenderIsClipEnabled');
  Pointer(SDL_SetRenderDrawBlendMode) := GetProcedureAddress(GSDL2, 'SDL_SetRenderDrawBlendMode');
  Pointer(SDL_SetRenderDrawColor) := GetProcedureAddress(GSDL2, 'SDL_SetRenderDrawColor');
  Pointer(SDL_SetRenderTarget) := GetProcedureAddress(GSDL2, 'SDL_SetRenderTarget');
  Pointer(SDL_CreateTexture) := GetProcedureAddress(GSDL2, 'SDL_CreateTexture');
  Pointer(SDL_CreateTextureFromSurface) := GetProcedureAddress(GSDL2, 'SDL_CreateTextureFromSurface');
  Pointer(SDL_DestroyTexture) := GetProcedureAddress(GSDL2, 'SDL_DestroyTexture');
  Pointer(SDL_LockTexture) := GetProcedureAddress(GSDL2, 'SDL_LockTexture');
  Pointer(SDL_UnlockTexture) := GetProcedureAddress(GSDL2, 'SDL_UnlockTexture');
  Pointer(SDL_UpdateTexture) := GetProcedureAddress(GSDL2, 'SDL_UpdateTexture');
  Pointer(SDL_SetTextureAlphaMod) := GetProcedureAddress(GSDL2, 'SDL_SetTextureAlphaMod');
  Pointer(SDL_SetTextureBlendMode) := GetProcedureAddress(GSDL2, 'SDL_SetTextureBlendMode');
  Pointer(SDL_SetTextureColorMod) := GetProcedureAddress(GSDL2, 'SDL_SetTextureColorMod');
  Pointer(SDL_FreeSurface) := GetProcedureAddress(GSDL2, 'SDL_FreeSurface');
  Pointer(SDL_GetNumDisplayModes) := GetProcedureAddress(GSDL2, 'SDL_GetNumDisplayModes');
  Pointer(SDL_GetDisplayMode) := GetProcedureAddress(GSDL2, 'SDL_GetDisplayMode');
  Pointer(SDL_GetDesktopDisplayMode) := GetProcedureAddress(GSDL2, 'SDL_GetDesktopDisplayMode');
  Pointer(SDL_GetCurrentDisplayMode) := GetProcedureAddress(GSDL2, 'SDL_GetCurrentDisplayMode');
  Pointer(SDL_PollEvent) := GetProcedureAddress(GSDL2, 'SDL_PollEvent');
  Pointer(SDL_PumpEvents) := GetProcedureAddress(GSDL2, 'SDL_PumpEvents');
  Pointer(SDL_StartTextInput) := GetProcedureAddress(GSDL2, 'SDL_StartTextInput');
  Pointer(SDL_StopTextInput) := GetProcedureAddress(GSDL2, 'SDL_StopTextInput');
  Pointer(SDL_GetKeyboardState) := GetProcedureAddress(GSDL2, 'SDL_GetKeyboardState');
  Pointer(SDL_GetModState) := GetProcedureAddress(GSDL2, 'SDL_GetModState');
  Pointer(SDL_GetMouseFocus) := GetProcedureAddress(GSDL2, 'SDL_GetMouseFocus');
  Pointer(SDL_GetMouseState) := GetProcedureAddress(GSDL2, 'SDL_GetMouseState');
  Pointer(SDL_WarpMouseInWindow) := GetProcedureAddress(GSDL2, 'SDL_WarpMouseInWindow');
  Pointer(SDL_ShowCursor) := GetProcedureAddress(GSDL2, 'SDL_ShowCursor');
  Pointer(SDL_NumJoysticks) := GetProcedureAddress(GSDL2, 'SDL_NumJoysticks');
  Pointer(SDL_JoystickOpen) := GetProcedureAddress(GSDL2, 'SDL_JoystickOpen');
  Pointer(SDL_JoystickUpdate) := GetProcedureAddress(GSDL2, 'SDL_JoystickUpdate');
  Pointer(SDL_JoystickNumAxes) := GetProcedureAddress(GSDL2, 'SDL_JoystickNumAxes');
  Pointer(SDL_JoystickNumButtons) := GetProcedureAddress(GSDL2, 'SDL_JoystickNumButtons');
  Pointer(SDL_JoystickGetAxis) := GetProcedureAddress(GSDL2, 'SDL_JoystickGetAxis');
  Pointer(SDL_JoystickGetButton) := GetProcedureAddress(GSDL2, 'SDL_JoystickGetButton');
  Pointer(SDL_JoystickClose) := GetProcedureAddress(GSDL2, 'SDL_JoystickClose');

  // TTF is optional: the console degrades to its non-TTF paths when absent.
  GTTF := LoadLibrary(TTF_LibName);
  if GTTF <> NilHandle then
  begin
    Pointer(TTF_Init) := GetProcedureAddress(GTTF, 'TTF_Init');
    Pointer(TTF_Quit) := GetProcedureAddress(GTTF, 'TTF_Quit');
    Pointer(TTF_OpenFont) := GetProcedureAddress(GTTF, 'TTF_OpenFont');
    Pointer(TTF_CloseFont) := GetProcedureAddress(GTTF, 'TTF_CloseFont');
    Pointer(TTF_SetFontStyle) := GetProcedureAddress(GTTF, 'TTF_SetFontStyle');
    Pointer(TTF_SizeUTF8) := GetProcedureAddress(GTTF, 'TTF_SizeUTF8');
    Pointer(TTF_RenderUTF8_Solid) := GetProcedureAddress(GTTF, 'TTF_RenderUTF8_Solid');
    Pointer(TTF_RenderUTF8_Blended) := GetProcedureAddress(GTTF, 'TTF_RenderUTF8_Blended');
  end;

  Result := True;
end;

function SDL2DynTTFAvailable: Boolean;
begin
  Result := GTTF <> NilHandle;
end;

end.
