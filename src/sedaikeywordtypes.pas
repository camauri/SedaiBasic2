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
unit SedaiKeywordTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SedaiLexerTypes, SedaiLexerToken;

type
  // === CONSTRAINT TYPES ===
  TKeywordConstraint = class
  public
    Keyword: string;
    RequiredNext: string;
    Optional: Boolean;
    ErrorMessage: string;

    constructor Create(const AKeyword, ARequiredNext: string;
                      AOptional: Boolean; const AErrorMsg: string);
  end;

  // === KEYWORD MATCHING STRUCTURES ===
  TKeywordMatchResult = record
    Found: Boolean;
    KeywordInfo: TKeywordInfo;
    MatchedLength: Integer;
    RemainingText: string;
    MatchType: TMatchType;
    Confidence: Single; // 0.0 to 1.0
  end;

  TKeywordSearchOptions = record
    CaseSensitive: Boolean;
    AllowPartialMatch: Boolean;
    RequireWordBoundary: Boolean;
    MaxResults: Integer;
    IncludeDeprecated: Boolean;
    CategoryFilter: string;
    MinConfidence: Single;
  end;

  // Array per risultati multipli (IDENTICI all'originale)
  TKeywordMatchArray = array of TKeywordMatchResult;
  TKeywordInfoArray = array of TKeywordInfo;


  // === MATCH CLASS ===
  TKeywordMatch = class
  private
    FKeywordInfo: TKeywordInfo;
    FMatchedText: string;
    FStartPos: Integer;
    FLength: Integer;
    FConfidence: Single;
    FMatchType: TMatchType;

  public
    constructor Create(AKeywordInfo: TKeywordInfo; const AMatchedText: string;
                      AStartPos, ALength: Integer; AConfidence: Single = 1.0);

    function ToString: string; override;
    function GetEndPos: Integer;
    function IsExactMatch: Boolean;
    function IsPartialMatch: Boolean;

    property KeywordInfo: TKeywordInfo read FKeywordInfo;
    property MatchedText: string read FMatchedText;
    property StartPos: Integer read FStartPos;
    property Length: Integer read FLength;
    property Confidence: Single read FConfidence;
    property MatchType: TMatchType read FMatchType;
    property EndPos: Integer read GetEndPos;
  end;

  // === EXCEPTIONS ===
  EKeywordRegistryException = class(Exception);
  EKeywordDuplicateException = class(EKeywordRegistryException);
  EKeywordNotFoundException = class(EKeywordRegistryException);
  EKeywordValidationException = class(EKeywordRegistryException);

implementation

{ TKeywordConstraint }

constructor TKeywordConstraint.Create(const AKeyword, ARequiredNext: string;
                                    AOptional: Boolean; const AErrorMsg: string);
begin
  inherited Create;
  Keyword := AKeyword;
  RequiredNext := ARequiredNext;
  Optional := AOptional;
  ErrorMessage := AErrorMsg;
end;

{ TKeywordMatch }

constructor TKeywordMatch.Create(AKeywordInfo: TKeywordInfo; const AMatchedText: string;
                               AStartPos, ALength: Integer; AConfidence: Single);
begin
  inherited Create;
  FKeywordInfo := AKeywordInfo;
  FMatchedText := AMatchedText;
  FStartPos := AStartPos;
  FLength := ALength;
  FConfidence := AConfidence;

  // Determine match type
  if ALength = System.Length(AKeywordInfo.Keyword) then
    FMatchType := mtExact
  else if ALength < System.Length(AKeywordInfo.Keyword) then
    FMatchType := mtPrefix
  else
    FMatchType := mtPartial;
end;

function TKeywordMatch.ToString: string;
var
  MatchTypeStr: string;
begin
  // First calculate the type string
  case FMatchType of
    mtExact: MatchTypeStr := 'EXACT';
    mtPrefix: MatchTypeStr := 'PREFIX';
    mtPartial: MatchTypeStr := 'PARTIAL';
    mtNone: MatchTypeStr := 'NONE';
  else
    MatchTypeStr := 'UNKNOWN';
  end;

  // Then use the string in Format
  Result := Format('Match[%s]: "%s" at %d (len=%d, conf=%.2f)', [
    MatchTypeStr,
    FMatchedText, FStartPos, FLength, FConfidence
  ]);
end;

function TKeywordMatch.GetEndPos: Integer;
begin
  Result := FStartPos + FLength - 1;
end;

function TKeywordMatch.IsExactMatch: Boolean;
begin
  Result := FMatchType = mtExact;
end;

function TKeywordMatch.IsPartialMatch: Boolean;
begin
  Result := FMatchType in [mtPrefix, mtPartial];
end;

end.
