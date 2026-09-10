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
unit SedaiTypeSizeProbe;

{ ⭐⭐⭐ THE ONE WIRE THAT LETS "#assert sizeof( T ) = 16" ANSWER WHAT THE COMPILER ANSWERS.

  fbc's preprocessor IS its compiler, so it reads a type's size out of the symbol table it is
  building. This pipeline preprocesses first and compiles after, so the directive is reached with
  nothing but text - and the two obvious ways out are both wrong:

    - leaving the check unmade is a DIVERGENCE, and the project's rule is full FreeBASIC;
    - writing a layout rule in the preprocessor is a SECOND COPY of UDTCLayout - alignment padding,
      "Field = n", bit-field runs, nested Union blocks, an inline fixed array member - a rule
      measured against fbc over eighty probes and already living in three copies inside SedaiSSA.
      A fourth, written on text, would drift the first time a width moved. This project has paid
      that bill once already: WIDE_CELL_BYTES was moved in one of its two readers and the report
      said 4n while the byte image still held 2n.

  ⇒ The preprocessor collects the declaration lines it has EMITTED so far - which is exactly what
  fbc's single pass holds in its symbol table at that point - and this unit compiles that fragment
  far enough to ask TSSAGenerator the ordinary SizeOf question. One layout rule, two askers.

  ⛔ IT IS A UNIT OF ITS OWN, AND A FUNCTION POINTER, BECAUSE OF A CYCLE. SedaiSSA already uses
  SedaiPreprocessor (for the "#undef" name list), so the preprocessor cannot name SedaiSSA back.
  This unit sits above both and installs the hook in its initialization. A front end that does not
  link it has no hook, and the question goes back to being unanswered - never wrongly answered. }

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

implementation

uses
  Classes, SysUtils,
  SedaiLexerFSM, SedaiTokenList, SedaiAST, SedaiParserResults, SedaiPackratParser,
  SedaiSSA, SedaiPreprocessor;

var
  GInProbe: Boolean = False;   // ⛔ re-entrancy: the fragment must never trigger another probe

function ProbeTypeSize(const DeclText, TypeName: string; out Sz: Int64): Boolean;
// Lex, parse and pre-scan a fragment made only of TYPE / UNION / ENUM / CONST declarations, then ask
// for the size of one of them. False when the fragment does not parse or the name is not a type
// declared in it - and False means "unanswered", never "zero".
var
  Lexer: TLexerFSM;
  Toks: TTokenList;
  Parser: TPackratParser;
  Res: TParsingResult;
  Gen: TSSAGenerator;
  Msg: string;
begin
  Sz := 0;
  Result := False;
  if GInProbe or (Trim(DeclText) = '') or (Trim(TypeName) = '') then Exit;
  GInProbe := True;
  Lexer := nil; Toks := nil; Parser := nil; Res := nil; Gen := nil;
  try
    try
      Lexer := TLexerFSM.Create;
      // ⚠️ MODERN, unconditionally, and it is not a guess: a "#assert sizeof(...)" is FreeBASIC
      // syntax and a fragment of bare TYPE declarations carries no line numbers, which is the only
      // thing the dialect probe reads. A CLASSIC program that reaches here asks the same question.
      Lexer.SetHasLineNumbers(False);
      Lexer.SetRequireSpacesBetweenTokens(True);
      Lexer.SetCaseSensitive(False);
      Lexer.Source := DeclText;
      Lexer.PreScanOptions;
      Toks := Lexer.ScanAllTokensFast;
      Parser := CreatePackratParser;
      Res := Parser.Parse(Toks);
      if GetEnvironmentVariable('PPSIZEDIAG') = '1' then
      begin
        Msg := '-';
        if (Res <> nil) and (Res.Errors.Count > 0) then Msg := Res.Errors[0].ToString;
        WriteLn(StdErr, '[ppsize:probe] parsed=', Ord((Res <> nil) and Res.Success),
                ' err=', Msg);
      end;
      if (Res <> nil) and Res.Success and (Res.AST <> nil) then
      begin
        Gen := TSSAGenerator.Create;
        Gen.ModernMode := True;
        Result := Gen.SizeOfDeclaredType(Res.AST, TypeName, Sz);
      end;
    except
      // A fragment is not a program: it can fail to parse for reasons that say nothing about the
      // user's file - a type that EXTENDS one declared inside a namespace this pass does not track,
      // a member whose default argument names something not collected. Unanswered, not an error.
      on E: Exception do
      begin
        if GetEnvironmentVariable('PPSIZEDIAG') = '1' then
          WriteLn(StdErr, '[ppsize:probe] EXCEPTION ', E.ClassName, ': ', E.Message);
        Result := False;
        Sz := 0;
      end;
    end;
  finally
    Gen.Free;
    Res.Free;
    Parser.Free;
    Lexer.Free;
    GInProbe := False;
  end;
end;

initialization
  GPPTypeSizeHook := @ProbeTypeSize;

finalization
  GPPTypeSizeHook := nil;

end.
