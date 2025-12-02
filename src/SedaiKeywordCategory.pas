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
unit SedaiKeywordCategory;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

type
  { TKeywordCategory - Categoria di keyword con metadata }
  TKeywordCategory = class
  private
    FName: string;
    FDescription: string;
    FKeywords: TStringList;
    FIsSystem: Boolean;
    FIcon: string;

  public
    constructor Create(const AName, ADescription: string; AIsSystem: Boolean = False);
    destructor Destroy; override;

    procedure AddKeyword(const Keyword: string);
    procedure RemoveKeyword(const Keyword: string);
    function HasKeyword(const Keyword: string): Boolean;
    function GetKeywordCount: Integer;

    property Name: string read FName;
    property Description: string read FDescription write FDescription;
    property Keywords: TStringList read FKeywords;
    property IsSystem: Boolean read FIsSystem;
    property Icon: string read FIcon write FIcon;
    property Count: Integer read GetKeywordCount;
  end;

implementation

{ TKeywordCategory }

constructor TKeywordCategory.Create(const AName, ADescription: string; AIsSystem: Boolean);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FIsSystem := AIsSystem;
  FKeywords := TStringList.Create;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FIcon := '';
end;

destructor TKeywordCategory.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TKeywordCategory.AddKeyword(const Keyword: string);
begin
  FKeywords.Add(UpperCase(Keyword));
end;

procedure TKeywordCategory.RemoveKeyword(const Keyword: string);
var
  Index: Integer;
begin
  Index := FKeywords.IndexOf(UpperCase(Keyword));
  if Index >= 0 then
    FKeywords.Delete(Index);
end;

function TKeywordCategory.HasKeyword(const Keyword: string): Boolean;
begin
  Result := FKeywords.IndexOf(UpperCase(Keyword)) >= 0;
end;

function TKeywordCategory.GetKeywordCount: Integer;
begin
  Result := FKeywords.Count;
end;

end.
