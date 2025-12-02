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
unit SedaiKeywordBase;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SedaiLexerTypes;

type
  // === CONSTRAINT CLASS ===
  TKeywordConstraint = class
  public
    Keyword: string;
    RequiredNext: string;
    Optional: Boolean;
    ErrorMessage: string;

    constructor Create(const AKeyword, ARequiredNext: string;
                      AOptional: Boolean; const AErrorMsg: string);
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

end.
