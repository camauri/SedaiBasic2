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
unit SedaiCStdio;

{ PRINT's bytes go through the C library's "stdout", the stream a C function the program calls writes to.

  ⭐ WHY (DIVERGENZE 434). FreeBASIC's runtime prints through C's stdio - fwrite to stdout, then fflush - so a
  PRINT and a printf/fputs of a C library the program calls share ONE stream, and with stdout and stderr in the
  same pipe they come out in the order the program wrote them. PRINT used to keep its bytes in a 64 KB buffer of
  its own (SedaiTerminalIO) while C wrote to its own buffer: "print one : fprintf(stderr, two) : print three :
  fputs(four, stdout) : print five" came out "two four one three five" where fbc writes "one two three four five".
  Same bytes, another order - and a program that logs through C and prints through BASIC reads scrambled.

  ⭐ HOW, and what it copies. ONE writer: the bytes go into C's buffer (fwrite_unlocked), so stdout against stdout is
  in order by construction. fbc then flushes after EVERY print, which puts one write system call on every PRINT; that
  is the part not copied, because it is not observable except against a stream C does NOT buffer - stderr - and only
  when C runs. So the flush happens where that can matter: before a call into C, when PRINT has written since the
  last flush (CStdoutSync - a test of one flag per call, and the system call only when printing and calling C
  really interleave).
  ⚠️ When stdout is not a terminal C's buffer is set to 64 KB (setvbuf), which is what PRINT had: glibc gives a pipe
  4 KB. A terminal keeps C's line buffering, and the terminal device flushes at every line as it did.
  ⚠️ Unix only. On Windows the stream is the C runtime's of the system, and the old path stays; SB_OUT_C=0 restores
  it here too, the A/B knob on one binary. }

{$mode ObjFPC}{$H+}
{$codepage UTF8}

interface

var
  // True once CStdioInit has decided PRINT goes through C's stdout. Read by SedaiTerminalIO on every write.
  GCStdioOn: Boolean = False;
  // PRINT wrote into C's buffer and nobody has flushed it since: a call into C must flush first.
  GCStdoutDirty: Boolean = False;

// Decide, once: on Unix, and unless SB_OUT_C=0. IsTerminal = stdout is a terminal (then C line-buffers it).
procedure CStdioInit(IsTerminal: Boolean);
// The bytes of a PRINT, into C's stdout.
procedure CStdoutWrite(P: PAnsiChar; L: SizeInt);
// Everything C holds for stdout, out now.
procedure CStdoutFlush;
// Before control passes to C: flush what PRINT left in the buffer, so C's own writes (to stderr above all) come after.
procedure CStdoutSync; inline;

implementation

uses
  SysUtils;

{$IFDEF UNIX}
function c_fwrite_unlocked(P: Pointer; Size, N: SizeUInt; F: Pointer): SizeUInt; cdecl; external 'c' name 'fwrite_unlocked';
function c_fflush(F: Pointer): LongInt; cdecl; external 'c' name 'fflush';
function c_setvbuf(F: Pointer; Buf: PAnsiChar; Mode: LongInt; Size: SizeUInt): LongInt; cdecl; external 'c' name 'setvbuf';
var
  c_stdout: Pointer; external 'c' name 'stdout';
const
  C_IOFBF = 0;   // glibc's _IOFBF
{$ENDIF}

var
  GInitDone: Boolean = False;

procedure CStdioInit(IsTerminal: Boolean);
begin
  if GInitDone then Exit;
  GInitDone := True;
  {$IFDEF UNIX}
  if SysUtils.GetEnvironmentVariable('SB_OUT_C') = '0' then Exit;
  // Before the first byte reaches the stream, or setvbuf is not allowed to change it.
  if not IsTerminal then c_setvbuf(c_stdout, nil, C_IOFBF, 65536);
  GCStdioOn := True;
  {$ENDIF}
end;

procedure CStdoutWrite(P: PAnsiChar; L: SizeInt);
begin
  {$IFDEF UNIX}
  if L <= 0 then Exit;
  c_fwrite_unlocked(P, 1, SizeUInt(L), c_stdout);
  GCStdoutDirty := True;
  {$ENDIF}
end;

procedure CStdoutFlush;
begin
  {$IFDEF UNIX}
  if GCStdioOn then c_fflush(c_stdout);
  {$ENDIF}
  GCStdoutDirty := False;
end;

procedure CStdoutSync; inline;
begin
  if GCStdoutDirty then CStdoutFlush;
end;

end.
