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
unit SedaiGenericTokenPool;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  SysUtils, Math,
  SedaiLexerTypes, SedaiLexerToken,
  SedaiMemoryUtils, SedaiMemoryConfig, SedaiLogging;

type
  // ===== POOL GENERICO CONFIGURABILE =====

  { TGenericTokenPool }

  TGenericTokenPool = class
  private
    FPool: array of TLexerToken;
    FPoolSize: Integer;
    FCurrentIndex: Integer;
    FTokensAllocated: Int64;
    FTokensReused: Int64;
    FOverflowCount: Integer;
    FSourceSize: Integer;

    // Configurable parameters
    FTokensPerCharRatio: Double;
    FSafetyFactor: Double;
    FMinPoolSize: Integer;
    FMaxPoolSize: Integer;

    procedure GrowPool(NewSize: Integer);

  public
    constructor Create(SourceSize: Integer;
                      TokensPerCharRatio: Double = 0.15;
                      SafetyFactor: Double = 2.0;
                      MinPoolSize: Integer = 100);
    destructor Destroy; override;

    function GetToken: TLexerToken; inline;
    procedure Reset;
    function GetStatistics: string;
    function NeedsGrowth: Boolean;
    function GetPoolSize: Integer;

    // Configurable properties
    property TokensPerCharRatio: Double read FTokensPerCharRatio write FTokensPerCharRatio;
    property SafetyFactor: Double read FSafetyFactor write FSafetyFactor;
    property MinPoolSize: Integer read FMinPoolSize write FMinPoolSize;
    property MaxPoolSize: Integer read FMaxPoolSize write FMaxPoolSize;
    property OverflowCount: Integer read FOverflowCount;
  end;

implementation

constructor TGenericTokenPool.Create(SourceSize: Integer;
                                   TokensPerCharRatio: Double;
                                   SafetyFactor: Double;
                                   MinPoolSize: Integer);
var
  i: Integer;
  EstimatedTokens: Integer;
  CalculatedSize: Integer;
  MaxAllowedSize: Integer;
begin
  inherited Create;

  // Set configurable parameters
  FSourceSize := SourceSize;
  FTokensPerCharRatio := TokensPerCharRatio;
  FSafetyFactor := SafetyFactor;
  FMinPoolSize := MinPoolSize;

  // Calculate maximum size based on available memory
  FMaxPoolSize := CalculateMaxTokensFromMemory(SizeOf(TLexerToken) + POOL_OVERHEAD_BYTES);

  // Pool size calculation
  if SourceSize <= 0 then
    CalculatedSize := FMinPoolSize
  else
  begin
    EstimatedTokens := Round(SourceSize * FTokensPerCharRatio);
    CalculatedSize := Round(EstimatedTokens * FSafetyFactor);
  end;

  // Applica limiti
  FPoolSize := Max(CalculatedSize, FMinPoolSize);
  FPoolSize := Min(FPoolSize, FMaxPoolSize);

  // Inizializza contatori
  FCurrentIndex := 0;
  FTokensAllocated := 0;
  FTokensReused := 0;
  FOverflowCount := 0;

  // Allocate pool
  SetLength(FPool, FPoolSize);

  // Pre-allocate all tokens
  for i := 0 to FPoolSize - 1 do
  begin
    FPool[i] := TLexerToken.Create(ttUnknown, '', 0, 0, 0, 0, nil);
    Inc(FTokensAllocated);
  end;

  Logger.Debug('Generic Token Pool Created:');
  Logger.Debug('  Source: %d chars', [SourceSize]);
  Logger.Debug('  Ratio: %.3f tokens/char', [FTokensPerCharRatio]);
  Logger.Debug('  Safety: %.1fx', [FSafetyFactor]);
  Logger.Debug('  Pool: %d tokens (%.1f MB)', [FPoolSize, (FPoolSize * SizeOf(TLexerToken)) / (1024*1024)]);
  Logger.Debug('  Limits: %d min, %d max', [FMinPoolSize, FMaxPoolSize]);
end;

destructor TGenericTokenPool.Destroy;
var
  i: Integer;
  EfficiencyPercent: Double;
  MemoryUsedMB: Double;
begin
  if FTokensAllocated > 0 then
  begin
    EfficiencyPercent := (FTokensReused / FTokensAllocated) * 100.0;
    MemoryUsedMB := (FPoolSize * SizeOf(TLexerToken)) / (1024 * 1024);

    Logger.Debug('Pool Statistics:');
    Logger.Debug('  Efficiency: %.1f%% (%d reused / %d allocated)',
      [EfficiencyPercent, FTokensReused, FTokensAllocated]);
    Logger.Debug('  Overflows: %d', [FOverflowCount]);
    Logger.Debug('  Memory: %.2f MB', [MemoryUsedMB]);
  end;

  for i := 0 to High(FPool) do
    FPool[i].Free;

  inherited Destroy;
end;

function TGenericTokenPool.GetToken: TLexerToken;
begin
  Result := FPool[FCurrentIndex];
  Inc(FTokensReused);

  // Circular buffer
  Inc(FCurrentIndex);
  if FCurrentIndex >= FPoolSize then
  begin
    FCurrentIndex := 0;
    Inc(FOverflowCount);
  end;
end;

procedure TGenericTokenPool.GrowPool(NewSize: Integer);
var
  OldSize, i: Integer;
begin
  if NewSize <= FPoolSize then Exit;
  if NewSize > FMaxPoolSize then NewSize := FMaxPoolSize;

  OldSize := FPoolSize;
  FPoolSize := NewSize;

  SetLength(FPool, FPoolSize);

  for i := OldSize to FPoolSize - 1 do
  begin
    FPool[i] := TLexerToken.Create(ttUnknown, '', 0, 0, 0, 0, nil);
    Inc(FTokensAllocated);
  end;

  Logger.Debug('Pool grown: %d -> %d tokens', [OldSize, FPoolSize]);
end;

function TGenericTokenPool.NeedsGrowth: Boolean;
begin
  // Cresci se abbiamo overflow frequenti E non siamo al limite massimo
  Result := (FOverflowCount > 3) and (FPoolSize < FMaxPoolSize);
end;

function TGenericTokenPool.GetPoolSize: Integer;
begin
  Result := FPoolSize;
end;

procedure TGenericTokenPool.Reset;
begin
  FCurrentIndex := 0;
  FOverflowCount := 0;
end;

function TGenericTokenPool.GetStatistics: string;
var
  EfficiencyPercent: Double;
  MemoryMB: Double;
begin
  if FTokensAllocated > 0 then
    EfficiencyPercent := (FTokensReused / FTokensAllocated) * 100.0
  else
    EfficiencyPercent := 0.0;

  MemoryMB := (FPoolSize * SizeOf(TLexerToken)) / (1024 * 1024);

  Result := Format('Pool: %d/%d tokens, %.1f%% eff, %d overflows, %.2f MB',
    [FCurrentIndex, FPoolSize, EfficiencyPercent, FOverflowCount, MemoryMB]);
end;

end.

