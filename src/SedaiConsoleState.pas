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
 *    Contact the author for licensing terms.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *}

{ Ownership of the *shared* Windows console state.

  A console's code page is a property of the console, not of the process attached to it. So
  SetConsoleOutputCP/SetConsoleCP change what the parent shell -- and everything that writes to that
  terminal afterwards -- will use, and the change OUTLIVES the process. sb.exe used to set both to
  UTF-8 unconditionally at startup and never restore them; a regression run launches the VM 744 times,
  so the calling terminal was left on a code page it never asked for and subsequent output rendered as
  mojibake.

  Two rules follow, and this unit enforces both:

    * Touch the console only when there IS one. Under a redirect (`sb prog.bas > out.txt`, the
      regression harness) or a pipe, the standard handles are files: nothing needs the console code
      page, so nothing should change it. StdOutIsConsole/StdInIsConsole answer that -- GetConsoleMode
      succeeds only on a real console handle.

    * Whatever you change, change back. RestoreConsoleCodePages runs from this unit's finalization, so
      it covers every normal exit and Halt without each entry point having to remember. It cannot cover
      TerminateProcess (that is what `timeout -s KILL` does to a hung run) -- which is the second reason
      the redirected case must not touch the console at all: a killed process restores nothing.

  Process-local encoding (SetTextCodePage, DefaultSystemCodePage) is deliberately NOT handled here: it
  affects only how this process encodes its own output, is invisible to the parent, and must stay set
  even when redirected so that captured bytes are identical either way. }

unit SedaiConsoleState;

{$mode objfpc}{$H+}

interface

{ True when the corresponding standard handle is attached to a real console (not a file or a pipe).
  Always False on non-Windows, where the callers below are no-ops. }
function StdOutIsConsole: Boolean;
function StdInIsConsole: Boolean;

{ Switch the console to UTF-8, remembering the previous code pages. No-op unless stdout is a console.
  Safe to call more than once: only the first call records the original code pages. }
procedure SetupConsoleUTF8;

{ Put the code pages back. Called automatically from this unit's finalization. }
procedure RestoreConsoleCodePages;

implementation

{$IFDEF WINDOWS}
uses
  Windows;

var
  FSaved: Boolean = False;
  FOldOutputCP: UINT = 0;
  FOldInputCP: UINT = 0;

function HandleIsConsole(StdHandle: DWORD): Boolean;
var
  H: THandle;
  Mode: DWORD;
begin
  H := GetStdHandle(StdHandle);
  Result := (H <> 0) and (H <> INVALID_HANDLE_VALUE) and GetConsoleMode(H, @Mode);
end;

function StdOutIsConsole: Boolean;
begin
  Result := HandleIsConsole(STD_OUTPUT_HANDLE);
end;

function StdInIsConsole: Boolean;
begin
  Result := HandleIsConsole(STD_INPUT_HANDLE);
end;

procedure SetupConsoleUTF8;
begin
  if not StdOutIsConsole then
    Exit;
  if not FSaved then
  begin
    FOldOutputCP := GetConsoleOutputCP;
    FOldInputCP := GetConsoleCP;
    FSaved := True;
  end;
  SetConsoleOutputCP(CP_UTF8);
  SetConsoleCP(CP_UTF8);
end;

procedure RestoreConsoleCodePages;
begin
  if not FSaved then
    Exit;
  if FOldOutputCP <> 0 then
    SetConsoleOutputCP(FOldOutputCP);
  if FOldInputCP <> 0 then
    SetConsoleCP(FOldInputCP);
  FSaved := False;
end;

{$ELSE}

function StdOutIsConsole: Boolean;
begin
  Result := False;
end;

function StdInIsConsole: Boolean;
begin
  Result := False;
end;

procedure SetupConsoleUTF8;
begin
end;

procedure RestoreConsoleCodePages;
begin
end;

{$ENDIF}

finalization
  RestoreConsoleCodePages;

end.
