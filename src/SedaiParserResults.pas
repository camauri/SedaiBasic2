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
unit SedaiParserResults;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, Contnrs,
  SedaiLexerTypes, SedaiLexerToken, SedaiParserTypes,
  SedaiAST, SedaiParserErrors;

type
  { TPackratCacheEntry - Essential memoization entry }
  TPackratCacheEntry = class
  private
    FAST: TASTNode;
    FTokensConsumed: Integer;
    FSuccess: Boolean;
    FError: TParserError;
    FHitCount: Integer;

  public
    constructor Create(AAST: TASTNode; ATokensConsumed: Integer; ASuccess: Boolean; AError: TParserError = nil);
    destructor Destroy; override;

    procedure IncrementHitCount; inline;

    property AST: TASTNode read FAST;
    property TokensConsumed: Integer read FTokensConsumed;
    property Success: Boolean read FSuccess;
    property Error: TParserError read FError;
    property HitCount: Integer read FHitCount;
  end;

  { TPackratCache - Streamlined memoization cache }
  TPackratCache = class(TFPHashObjectList)
  private
    FMaxEntries: Integer;
    FHitCount: Integer;
    FMissCount: Integer;
    FEvictionCount: Integer;

    procedure EvictOldEntries;
    function GetCacheKey(const RuleName: string; TokenIndex: Integer): string; inline;

  public
    constructor Create(AMaxEntries: Integer = 1000);

    // Core cache operations (hot path)
    function GetEntry(const RuleName: string; TokenIndex: Integer): TPackratCacheEntry; inline;
    procedure SetEntry(const RuleName: string; TokenIndex: Integer; Entry: TPackratCacheEntry); inline;
    function HasEntry(const RuleName: string; TokenIndex: Integer): Boolean; inline;

    // Essential management
    procedure ClearCache;
    function GetHitRate: Double; inline;
    function GetStatistics: string;

    property MaxEntries: Integer read FMaxEntries write FMaxEntries;
    property HitCount: Integer read FHitCount;
    property MissCount: Integer read FMissCount;
  end;

  { TParsingResult - Essential parsing result }
  TParsingResult = class
  private
    FAST: TASTNode;
    FSuccess: Boolean;
    FErrors: TParserErrorList;
    FTokensConsumed: Integer;
    FParsingTime: Double;
    FSourceFile: string;

    // Essential statistics
    FNodesCreated: Integer;
    FMaxRecursionDepth: Integer;

  public
    constructor Create;
    destructor Destroy; override;

    // Error management (essential only)
    procedure AddError(Error: TParserError); overload;
    procedure AddError(const Message: string; Token: TLexerToken); overload;
    procedure AddWarning(const Message: string; Token: TLexerToken);

    // Result validation
    function IsValid: Boolean; inline;
    function CanExecute: Boolean; inline; // True if no fatal errors

    // Essential statistics
    procedure UpdateStatistics(NodesCreated, MaxRecursion: Integer);

    // Minimal reporting (for debugging only)
    function GetErrorSummary: string;

    // Properties
    property AST: TASTNode read FAST write FAST;
    property Success: Boolean read FSuccess write FSuccess;
    property Errors: TParserErrorList read FErrors;
    property TokensConsumed: Integer read FTokensConsumed write FTokensConsumed;
    property ParsingTime: Double read FParsingTime write FParsingTime;
    property SourceFile: string read FSourceFile write FSourceFile;
    property NodesCreated: Integer read FNodesCreated write FNodesCreated;
    property MaxRecursionDepth: Integer read FMaxRecursionDepth write FMaxRecursionDepth;
  end;

implementation

uses
  Math;

{ TPackratCacheEntry }

constructor TPackratCacheEntry.Create(AAST: TASTNode; ATokensConsumed: Integer; ASuccess: Boolean; AError: TParserError);
begin
  inherited Create;
  FAST := AAST;
  FTokensConsumed := ATokensConsumed;
  FSuccess := ASuccess;
  FError := AError;
  FHitCount := 0;
end;

destructor TPackratCacheEntry.Destroy;
begin
  // Don't free AST - it's owned by the parsing result
  // Don't free Error - it's owned by the error list
  inherited Destroy;
end;

procedure TPackratCacheEntry.IncrementHitCount;
begin
  Inc(FHitCount);
end;

{ TPackratCache }

constructor TPackratCache.Create(AMaxEntries: Integer);
begin
  inherited Create(True); // Own objects
  FMaxEntries := AMaxEntries;
  FHitCount := 0;
  FMissCount := 0;
  FEvictionCount := 0;
end;

function TPackratCache.GetCacheKey(const RuleName: string; TokenIndex: Integer): string;
begin
  Result := RuleName + '@' + IntToStr(TokenIndex);
end;

procedure TPackratCache.EvictOldEntries;
begin
  // Simple eviction: remove oldest entries when cache is full
  while Count > FMaxEntries do
  begin
    Delete(0); // Remove first (oldest) entry
    Inc(FEvictionCount);
  end;
end;

function TPackratCache.GetEntry(const RuleName: string; TokenIndex: Integer): TPackratCacheEntry;
var
  Key: string;
begin
  Key := GetCacheKey(RuleName, TokenIndex);
  Result := TPackratCacheEntry(Find(Key));

  if Assigned(Result) then
  begin
    Inc(FHitCount);
    Result.IncrementHitCount;
  end
  else
    Inc(FMissCount);
end;

procedure TPackratCache.SetEntry(const RuleName: string; TokenIndex: Integer; Entry: TPackratCacheEntry);
var
  Key: string;
begin
  Key := GetCacheKey(RuleName, TokenIndex);
  Add(Key, Entry);

  // Check if we need to evict old entries
  if Count > FMaxEntries then
    EvictOldEntries;
end;

function TPackratCache.HasEntry(const RuleName: string; TokenIndex: Integer): Boolean;
var
  Key: string;
begin
  Key := GetCacheKey(RuleName, TokenIndex);
  Result := Find(Key) <> nil;
end;

procedure TPackratCache.ClearCache;
begin
  Clear;
  FHitCount := 0;
  FMissCount := 0;
  FEvictionCount := 0;
end;

function TPackratCache.GetHitRate: Double;
begin
  if FHitCount + FMissCount > 0 then
    Result := (FHitCount * 100.0) / (FHitCount + FMissCount)
  else
    Result := 0.0;
end;

function TPackratCache.GetStatistics: string;
begin
  Result := Format('Cache: %d/%d entries, %.1f%% hit rate, %d evictions',
    [Count, FMaxEntries, GetHitRate, FEvictionCount]);
end;

{ TParsingResult }

constructor TParsingResult.Create;
begin
  inherited Create;
  FAST := nil;
  FSuccess := False;
  FErrors := TParserErrorList.Create;
  FTokensConsumed := 0;
  FParsingTime := 0.0;
  FSourceFile := '';
  FNodesCreated := 0;
  FMaxRecursionDepth := 0;
end;

destructor TParsingResult.Destroy;
begin
  FErrors.Free;
  // Don't free AST here - let the caller manage it
  inherited Destroy;
end;

procedure TParsingResult.AddError(Error: TParserError);
begin
  FErrors.AddError(Error);
  FSuccess := False;
end;

procedure TParsingResult.AddError(const Message: string; Token: TLexerToken);
begin
  FErrors.AddError(Message, Token);
  FSuccess := False;
end;

procedure TParsingResult.AddWarning(const Message: string; Token: TLexerToken);
begin
  FErrors.AddWarning(Message, Token);
  // Warnings don't affect success status
end;

function TParsingResult.IsValid: Boolean;
begin
  Result := FSuccess and Assigned(FAST) and not FErrors.HasFatals;
end;

function TParsingResult.CanExecute: Boolean;
begin
  Result := not FErrors.HasFatals;
end;

procedure TParsingResult.UpdateStatistics(NodesCreated, MaxRecursion: Integer);
begin
  FNodesCreated := NodesCreated;
  FMaxRecursionDepth := MaxRecursion;
end;

function TParsingResult.GetErrorSummary: string;
begin
  if FErrors.Count = 0 then
    Result := 'No errors'
  else
    Result := Format('%d errors (%d warnings, %d errors, %d fatal)',
      [FErrors.Count, FErrors.WarningCount, FErrors.ErrorCount, FErrors.FatalCount]);
end;

end.
