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
unit SedaiPackratCore;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  SedaiParserTypes, SedaiAST, SedaiParserErrors,
  SedaiParserContext, SedaiParserResults;

type
  TSimpleParseFunc = function: TASTNode of object;
  TContextParseFunc = function(Context: TParserContext): TASTNode of object;

  // === TIER 1 OPTIMIZATION: Adaptive Memoization ===
  TMemoizationMode = (
    mmAlways,      // Always memoize (default, for complex grammars)
    mmAdaptive,    // Smart: memoize only recursive/complex rules
    mmNever        // Never memoize (for benchmarking/testing only)
  );

  { TPackratCore - Core memoization and parsing infrastructure }
  TPackratCore = class
  private
    FContext: TParserContext;
    FMemoizationMode: TMemoizationMode;
    FMemoizationThreshold: Integer;  // Recursion depth threshold for adaptive mode
    {$IFDEF DEBUG}
    FDebugMode: Boolean;
    FBypassedMemoizations: Integer;  // Statistics: memoizations skipped
    procedure LogDebug(const Msg: string); inline;
    {$ENDIF}

    // === TIER 1: Adaptive memoization heuristic ===
    function ShouldMemoize(const RuleName: string): Boolean; inline;

  protected
    // Virtual methods essential for extensibility
    procedure DoNodeCreated(Node: TASTNode); virtual;
    procedure DoError(const Message: string; Token: TLexerToken); virtual;
    procedure DoCacheHit(const RuleName: string; TokenIndex: Integer); virtual;
    procedure DoCacheMiss(const RuleName: string; TokenIndex: Integer); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    // === CORE MEMOIZATION (hot path) ===
    function Memoize(const RuleName: string; ParseFunc: TSimpleParseFunc): TASTNode;
    function MemoizeWithContext(const RuleName: string; ParseFunc: TContextParseFunc): TASTNode;

    // === CONTEXT MANAGEMENT ===
    procedure SetContext(AContext: TParserContext); virtual;
    function GetContext: TParserContext; inline;
    function HasValidContext: Boolean; inline;

    // === ESSENTIAL OPERATIONS ===
    procedure ClearCache;
    procedure HandleError(const Message: string; Token: TLexerToken);
    function CreateErrorNode(const Message: string; Token: TLexerToken): TASTNode;

    // === MINIMAL CONFIGURATION ===
    {$IFDEF DEBUG}
    procedure EnableDebugMode(Verbose: Boolean = False);
    procedure DisableDebugMode;
    {$ENDIF}

    // === ESSENTIAL STATISTICS (for performance tuning only) ===
    function GetCacheHitRate: Double;
    function GetCacheStatistics: string;

    // === PROPERTIES ===
    property Context: TParserContext read FContext;

    // === TIER 1: Memoization control ===
    property MemoizationMode: TMemoizationMode read FMemoizationMode write FMemoizationMode;
    property MemoizationThreshold: Integer read FMemoizationThreshold write FMemoizationThreshold;

    {$IFDEF DEBUG}
    property DebugMode: Boolean read FDebugMode;
    property BypassedMemoizations: Integer read FBypassedMemoizations;
    {$ENDIF}

  end;

implementation

uses
  Math;

{ TPackratCore }

constructor TPackratCore.Create;
begin
  inherited Create;
  FContext := nil;

  // === TIER 1: Initialize adaptive memoization (default: always memoize) ===
  FMemoizationMode := mmAlways;
  FMemoizationThreshold := 3;  // Enable memoization after 3 recursion levels

  {$IFDEF DEBUG}
  FDebugMode := False;
  FBypassedMemoizations := 0;
  LogDebug('TPackratCore created');
  {$ENDIF}
end;

destructor TPackratCore.Destroy;
begin
  {$IFDEF DEBUG}
  LogDebug('TPackratCore destroyed');
  {$ENDIF}

  inherited Destroy;
end;

{$IFDEF DEBUG}
procedure TPackratCore.LogDebug(const Msg: string);
begin
  if FDebugMode then
    WriteLn('DEBUG [PackratCore]: ', Msg);
end;
{$ENDIF}


// === TIER 1: Adaptive Memoization Heuristic ===
function TPackratCore.ShouldMemoize(const RuleName: string): Boolean;
begin
  Result := False;  // Initialize result to avoid warning

  case FMemoizationMode of
    mmNever:
      begin
        Result := False;
        {$IFDEF DEBUG}
        Inc(FBypassedMemoizations);
        {$ENDIF}
      end;

    mmAlways:
      Result := True;

    mmAdaptive:
      begin
        // Memoize ONLY for recursive/complex rules or deep recursion
        // Most BASIC statements are linear and don't need memoization
        Result := (RuleName = 'Expression') or           // Recursive (Pratt parser)
                  (RuleName = 'ExpressionStatement') or  // May contain complex expression
                  (RuleName = 'IfStatement') or          // Multi-branch conditionals
                  (RuleName = 'ForStatement') or         // Complex loop structure
                  (RuleName = 'WhileStatement') or       // Loop with condition
                  (RuleName = 'DoStatement') or          // DO-LOOP structure
                  (FContext.RecursionDepth >= FMemoizationThreshold);

        {$IFDEF DEBUG}
        if not Result then
          Inc(FBypassedMemoizations);
        {$ENDIF}
      end;
  end;
end;

// === CORE MEMOIZATION ===

function TPackratCore.Memoize(const RuleName: string; ParseFunc: TSimpleParseFunc): TASTNode;
var
  StartIndex: Integer;
  CacheEntry: TPackratCacheEntry;
  TokensConsumed: Integer;
  ParseError: TParserError;
begin
  if not HasValidContext then
  begin
    {$IFDEF DEBUG}
    LogDebug('Memoize called without valid context');
    {$ENDIF}

    Result := nil;
    Exit;
  end;

  // === TIER 1: FAST PATH - Bypass memoization for simple statements ===
  if not ShouldMemoize(RuleName) then
  begin
    // Direct parsing without cache overhead
    if not FContext.EnterRecursion then
    begin
      Result := CreateErrorNode('Recursion limit exceeded', FContext.CurrentToken);
      Exit;
    end;

    try
      Result := ParseFunc();
    finally
      FContext.ExitRecursion;
    end;

    Exit;
  end;

  // === SLOW PATH: Full memoization (original code) ===
  StartIndex := FContext.CurrentIndex;

  // Check cache first (hot path)
  CacheEntry := FContext.GetCacheEntry(RuleName, StartIndex);
  if Assigned(CacheEntry) then
  begin
    DoCacheHit(RuleName, StartIndex);
    // Restore parser position
    FContext.CurrentIndex := StartIndex + CacheEntry.TokensConsumed;
    Result := CacheEntry.AST;
    Exit;
  end;

  DoCacheMiss(RuleName, StartIndex);

  // Enter recursion guard
  if not FContext.EnterRecursion then
  begin
    Result := CreateErrorNode('Recursion limit exceeded', FContext.CurrentToken);
    Exit;
  end;

  try
    // Parse and cache result
    Result := ParseFunc();
    TokensConsumed := FContext.CurrentIndex - StartIndex;

    // Create cache entry
    ParseError := nil;
    if FContext.InPanicMode then
      ParseError := TParserError.Create('Parse error during memoization', FContext.CurrentToken);

    CacheEntry := TPackratCacheEntry.Create(Result, TokensConsumed, Assigned(Result), ParseError);
    FContext.SetCacheEntry(RuleName, StartIndex, CacheEntry);

  finally
    FContext.ExitRecursion;
  end;
end;

function TPackratCore.MemoizeWithContext(const RuleName: string; ParseFunc: TContextParseFunc): TASTNode;
var
  StartIndex: Integer;
  CacheEntry: TPackratCacheEntry;
  TokensConsumed: Integer;
  ParseError: TParserError;
begin
  if not HasValidContext then
  begin
    {$IFDEF DEBUG}
    LogDebug('MemoizeWithContext called without valid context');
    {$ENDIF}

    Result := nil;
    Exit;
  end;

  // === TIER 1: FAST PATH - Bypass memoization for simple statements ===
  if not ShouldMemoize(RuleName) then
  begin
    // Direct parsing without cache overhead
    if not FContext.EnterRecursion then
    begin
      Result := CreateErrorNode('Recursion limit exceeded', FContext.CurrentToken);
      Exit;
    end;

    try
      Result := ParseFunc(FContext);
    finally
      FContext.ExitRecursion;
    end;

    Exit;
  end;

  // === SLOW PATH: Full memoization (original code) ===
  StartIndex := FContext.CurrentIndex;

  // Check cache first (hot path)
  CacheEntry := FContext.GetCacheEntry(RuleName, StartIndex);
  if Assigned(CacheEntry) then
  begin
    DoCacheHit(RuleName, StartIndex);
    FContext.CurrentIndex := StartIndex + CacheEntry.TokensConsumed;
    Result := CacheEntry.AST;
    Exit;
  end;

  DoCacheMiss(RuleName, StartIndex);

  // Enter recursion guard
  if not FContext.EnterRecursion then
  begin
    Result := CreateErrorNode('Recursion limit exceeded', FContext.CurrentToken);
    Exit;
  end;

  try
    // Parse with context and cache result
    Result := ParseFunc(FContext);
    TokensConsumed := FContext.CurrentIndex - StartIndex;

    // Create cache entry
    ParseError := nil;
    if FContext.InPanicMode then
      ParseError := TParserError.Create('Parse error during memoization', FContext.CurrentToken);

    CacheEntry := TPackratCacheEntry.Create(Result, TokensConsumed, Assigned(Result), ParseError);
    FContext.SetCacheEntry(RuleName, StartIndex, CacheEntry);

  finally
    FContext.ExitRecursion;
  end;
end;

// === CONTEXT MANAGEMENT ===

procedure TPackratCore.SetContext(AContext: TParserContext);
begin
  FContext := AContext;
  {$IFDEF DEBUG}
  if Assigned(FContext) then
    FContext.DebugMode := FDebugMode;
  {$ENDIF}
end;

function TPackratCore.GetContext: TParserContext;
begin
  Result := FContext;
end;

function TPackratCore.HasValidContext: Boolean;
begin
  Result := Assigned(FContext) and FContext.IsValid;
end;

// === ESSENTIAL OPERATIONS ===

procedure TPackratCore.ClearCache;
begin
  if Assigned(FContext) then
    FContext.ClearCache;

  {$IFDEF DEBUG}
  LogDebug('Cache cleared');
  {$ENDIF}
end;

procedure TPackratCore.HandleError(const Message: string; Token: TLexerToken);
begin
  if Assigned(FContext) then
  begin
    FContext.AddError(Message, Token);
    DoError(Message, Token);
  end;

  {$IFDEF DEBUG}
  LogDebug(Format('Parse error: %s', [Message]));
  {$ENDIF}
end;

function TPackratCore.CreateErrorNode(const Message: string; Token: TLexerToken): TASTNode;
begin
  Result := TASTNode.CreateWithValue(antError, Message, Token);
  DoNodeCreated(Result);
end;

// === MINIMAL CONFIGURATION ===

{$IFDEF DEBUG}
procedure TPackratCore.EnableDebugMode(Verbose: Boolean);
begin
  FDebugMode := True;
  if Assigned(FContext) then
    FContext.DebugMode := True;
  LogDebug('Debug mode enabled');
end;

procedure TPackratCore.DisableDebugMode;
begin
  LogDebug('Debug mode disabled');
  FDebugMode := False;
  if Assigned(FContext) then
    FContext.DebugMode := False;
end;
{$ENDIF}


// === ESSENTIAL STATISTICS ===

function TPackratCore.GetCacheHitRate: Double;
begin
  if Assigned(FContext) then
    Result := FContext.Cache.GetHitRate
  else
    Result := 0.0;
end;

function TPackratCore.GetCacheStatistics: string;
begin
  if Assigned(FContext) then
  begin
    Result := FContext.Cache.GetStatistics;
    {$IFDEF DEBUG}
    Result := Result + Format(#13#10'Memoization Mode: %s', [
      case FMemoizationMode of
        mmAlways: 'Always';
        mmAdaptive: 'Adaptive';
        mmNever: 'Never';
      end]);
    Result := Result + Format(#13#10'Bypassed Memoizations: %d', [FBypassedMemoizations]);
    {$ENDIF}
  end
  else
    Result := 'No context available';
end;

// === VIRTUAL EVENT METHODS ===

procedure TPackratCore.DoNodeCreated(Node: TASTNode);
begin
  if Assigned(FContext) then
    FContext.IncrementNodesCreated;
  // Override in subclasses for custom node handling
end;

procedure TPackratCore.DoError(const Message: string; Token: TLexerToken);
begin
  // Override in subclasses for custom error handling
end;

procedure TPackratCore.DoCacheHit(const RuleName: string; TokenIndex: Integer);
begin
  // Override in subclasses for cache hit notifications
end;

procedure TPackratCore.DoCacheMiss(const RuleName: string; TokenIndex: Integer);
begin
  // Override in subclasses for cache miss notifications
end;

end.
