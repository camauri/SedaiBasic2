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
unit SedaiMemoryUtils;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Process,
  {$ENDIF}
  SysUtils, Classes,
  SedaiMemoryConfig, SedaiLogging;

// ===== FUNZIONI MEMORIA =====
function GetAvailableMemoryMB: Integer;
function CalculateMaxTokensFromMemory(TokenSizeBytes: Integer): Integer;

implementation

function GetAvailableMemoryMB: Integer;
{$IFDEF WINDOWS}
var
  MemStatus: TMemoryStatus;
{$ENDIF}
{$IFDEF LINUX}
var
  Process: TProcess;
  MemInfo: TStringList;
  Line: string;
  i: Integer;
{$ENDIF}
begin
  Result := MEMORY_DEFAULT_MB;

  try
    {$IFDEF WINDOWS}
    MemStatus.dwLength := SizeOf(MemStatus);
    GlobalMemoryStatus(MemStatus);

    Result := Round((MemStatus.dwAvailPhys / (1024 * 1024)) * MEMORY_USAGE_PERCENT);
    {$ENDIF}

    {$IFDEF LINUX}
    Process := TProcess.Create(nil);
    try
      Process.CommandLine := 'cat /proc/meminfo';
      Process.Options := [poWaitOnExit, poUsePipes];
      Process.Execute;

      MemInfo := TStringList.Create;
      try
        MemInfo.LoadFromStream(Process.Output);
        for i := 0 to MemInfo.Count - 1 do
        begin
          Line := MemInfo[i];
          if Pos('MemAvailable:', Line) > 0 then
          begin
            // Extract value in KB and convert to MB
            Line := StringReplace(Line, 'MemAvailable:', '', []);
            Line := StringReplace(Line, 'kB', '', []);
            Line := Trim(Line);
            Result := Round(StrToIntDef(Line, LINUX_DEFAULT_MEMINFO_KB) / 1024 * MEMORY_USAGE_PERCENT);
            Break;
          end;
        end;
      finally
        MemInfo.Free;
      end;
    finally
      Process.Free;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      Result := MEMORY_DEFAULT_MB;
    end;
  end;

  // Clamp a valori ragionevoli
  if Result < MEMORY_MIN_MB then Result := MEMORY_MIN_MB;
  if Result > MEMORY_MAX_MB then Result := MEMORY_MAX_MB;
end;

function CalculateMaxTokensFromMemory(TokenSizeBytes: Integer): Integer;
var
  AvailableMemoryMB: Integer;
  MaxMemoryForPoolMB: Integer;
begin
  AvailableMemoryMB := GetAvailableMemoryMB;

  // Use at most half of the available memory for the pool
  MaxMemoryForPoolMB := AvailableMemoryMB div 2;

  // Calculate how many tokens we can afford
  if TokenSizeBytes <= 0 then
    TokenSizeBytes := SizeOf(Pointer) + POOL_OVERHEAD_BYTES;

  Result := (MaxMemoryForPoolMB * 1024 * 1024) div TokenSizeBytes;

  // Clamp a valori ragionevoli
  if Result < POOL_MIN_TOKENS then Result := POOL_MIN_TOKENS;
  if Result > POOL_MAX_TOKENS then Result := POOL_MAX_TOKENS;
  Logger.Debug('Memory-based pool limit: %d MB available -> max %d tokens',
    [AvailableMemoryMB, Result]);
end;

end.
