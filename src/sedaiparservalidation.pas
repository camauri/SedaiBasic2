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
unit SedaiParserValidation;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,
  SedaiLexerTypes, SedaiLexerToken, SedaiParserTypes,
  SedaiAST, SedaiParserContext;

type
  // === VALIDATION STACK ENTRIES ===
  TIfStackEntry = record
    IfNode: TASTNode;          // The IF node
    ThenNode: TASTNode;        // The THEN node (when found)
    HasElse: Boolean;          // Whether it already has an ELSE
    HasThenBlock: Boolean;     // Whether THEN has a BEGIN block
    HasElseBlock: Boolean;     // Whether ELSE has a BEGIN block
    LineNumber: Integer;       // Line number for error debugging
    TokenIndex: Integer;       // Position in token stream
  end;

  TLoopStackEntry = record
    LoopType: TTokenType;      // ttLoopBlockStart (FOR, WHILE, DO)
    LoopNode: TASTNode;        // The loop node
    LoopKeyword: string;       // 'FOR', 'WHILE', 'DO'
    ExpectedEnd: string;       // 'NEXT', 'WEND', 'LOOP'
    LineNumber: Integer;       // Line number for debug
    TokenIndex: Integer;       // Position in token stream
  end;

  TGosubStackEntry = record
    GosubNode: TASTNode;       // The GOSUB node
    LineNumber: Integer;       // Line number for debug
    TokenIndex: Integer;       // Position in token stream
  end;

  TBlockStackEntry = record
    BlockType: TTokenType;     // ttBlockBegin
    BlockNode: TASTNode;       // The BEGIN node
    ExpectedEnd: string;       // 'END', 'BEND'
    LineNumber: Integer;       // Line number for debug
    TokenIndex: Integer;       // Position in token stream
  end;

  // === VALIDATION STACKS ===
  TIfStack = array of TIfStackEntry;
  TLoopStack = array of TLoopStackEntry;
  TGosubStack = array of TGosubStackEntry;
  TBlockStack = array of TBlockStackEntry;

  // === VALIDATION MANAGER ===
  TParserValidationStacks = class
  private
    FContext: TParserContext;  // Reference al context per errori
    FIfStack: TIfStack;
    FLoopStack: TLoopStack;
    FGosubStack: TGosubStack;
    FBlockStack: TBlockStack;

  public
    constructor Create(AContext: TParserContext);
    destructor Destroy; override;

    // === IF/THEN/ELSE VALIDATION ===
    procedure PushIf(IfNode: TASTNode; TokenIndex: Integer);
    function PopIf: Boolean;
    function GetCurrentIf: TIfStackEntry;
    function HasActiveIf: Boolean;
    procedure SetThenForCurrentIf(ThenNode: TASTNode);
    procedure SetElseForCurrentIf;
    procedure SetThenBlockForCurrentIf;
    procedure SetElseBlockForCurrentIf;
    procedure ClearThenBlockForCurrentIf;
    procedure ClearElseBlockForCurrentIf;
    function CanPopIfAtEOL: Boolean;
    function ValidateElse: Boolean;  // ELSE senza THEN
    function ValidateThen: Boolean;  // THEN senza IF

    // === LOOP VALIDATION ===
    procedure PushLoop(LoopType: TTokenType; LoopNode: TASTNode;
                       const LoopKeyword, ExpectedEnd: string; TokenIndex: Integer);
    function PopLoop(const EndKeyword: string): Boolean;
    function GetCurrentLoop: TLoopStackEntry;
    function HasActiveLoop: Boolean;
    function ValidateLoopEnd(const EndKeyword: string): Boolean;

    // === GOSUB/RETURN VALIDATION ===
    procedure PushGosub(GosubNode: TASTNode; TokenIndex: Integer);
    function PopGosub: Boolean;
    function HasActiveGosub: Boolean;
    function ValidateReturn: Boolean;  // RETURN senza GOSUB

    // === BLOCK VALIDATION ===
    procedure PushBlock(BlockType: TTokenType; BlockNode: TASTNode;
                        const ExpectedEnd: string; TokenIndex: Integer);
    function PopBlock(const EndKeyword: string): Boolean;
    function HasActiveBlock: Boolean;
    function ValidateBlockEnd(const EndKeyword: string): Boolean;

    // === VALIDATION AT END OF PARSING ===
    function ValidateAllClosed: Boolean;
    procedure ReportUnclosedConstructs;

    // === UTILITIES ===
    procedure ClearAllStacks;
    function GetStackInfo: string;  // Debug info

    // === PROPERTIES ===
    property Context: TParserContext read FContext;
  end;

implementation

{ TParserValidationStacks }

constructor TParserValidationStacks.Create(AContext: TParserContext);
begin
  inherited Create;
  FContext := AContext;
  SetLength(FIfStack, 0);
  SetLength(FLoopStack, 0);
  SetLength(FGosubStack, 0);
  SetLength(FBlockStack, 0);
end;

destructor TParserValidationStacks.Destroy;
begin
  ClearAllStacks;
  inherited Destroy;
end;

// === IF/THEN/ELSE VALIDATION ===

procedure TParserValidationStacks.PushIf(IfNode: TASTNode; TokenIndex: Integer);
var
  Entry: TIfStackEntry;
begin
  Entry.IfNode := IfNode;
  Entry.ThenNode := nil;
  Entry.HasElse := False;
  Entry.HasThenBlock := False;  // *** AGGIUNTO ***
  Entry.HasElseBlock := False;  // *** AGGIUNTO ***
  Entry.TokenIndex := TokenIndex;

  if Assigned(IfNode) and Assigned(IfNode.Token) then
    Entry.LineNumber := IfNode.Token.Line
  else
    Entry.LineNumber := 0;

  SetLength(FIfStack, Length(FIfStack) + 1);
  FIfStack[High(FIfStack)] := Entry;
end;

function TParserValidationStacks.PopIf: Boolean;
begin
  Result := Length(FIfStack) > 0;
  if Result then
    SetLength(FIfStack, Length(FIfStack) - 1);
end;

function TParserValidationStacks.GetCurrentIf: TIfStackEntry;
begin
  if Length(FIfStack) > 0 then
    Result := FIfStack[High(FIfStack)]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TParserValidationStacks.HasActiveIf: Boolean;
begin
  Result := Length(FIfStack) > 0;
end;

procedure TParserValidationStacks.SetThenForCurrentIf(ThenNode: TASTNode);
begin
  if Length(FIfStack) > 0 then
    FIfStack[High(FIfStack)].ThenNode := ThenNode;
end;

procedure TParserValidationStacks.SetElseForCurrentIf;
begin
  if Length(FIfStack) > 0 then
    FIfStack[High(FIfStack)].HasElse := True;
end;

// *** NUOVI METODI AGGIUNTI ***

procedure TParserValidationStacks.SetThenBlockForCurrentIf;
begin
  if Length(FIfStack) > 0 then
    FIfStack[High(FIfStack)].HasThenBlock := True;
end;

procedure TParserValidationStacks.SetElseBlockForCurrentIf;
begin
  if Length(FIfStack) > 0 then
    FIfStack[High(FIfStack)].HasElseBlock := True;
end;

procedure TParserValidationStacks.ClearThenBlockForCurrentIf;
begin
  if Length(FIfStack) > 0 then
    FIfStack[High(FIfStack)].HasThenBlock := False;
end;

procedure TParserValidationStacks.ClearElseBlockForCurrentIf;
begin
  if Length(FIfStack) > 0 then
    FIfStack[High(FIfStack)].HasElseBlock := False;
end;

function TParserValidationStacks.CanPopIfAtEOL: Boolean;
var
  CurrentIf: TIfStackEntry;
begin
  Result := False;

  if not HasActiveIf then
    Exit;

  CurrentIf := GetCurrentIf;

  // Can only Pop if:
  // 1. Has a THEN
  // 2. If has ELSE, no active blocks
  // 3. If no ELSE, no active THEN blocks

  if not Assigned(CurrentIf.ThenNode) then
    Exit; // No THEN, cannot close

  if CurrentIf.HasElse then
  begin
    // IF...THEN...ELSE: can only close if ELSE has no active blocks
    Result := not CurrentIf.HasElseBlock;
  end
  else
  begin
    // IF...THEN: can only close if THEN has no active blocks
    Result := not CurrentIf.HasThenBlock;
  end;
end;

function TParserValidationStacks.ValidateElse: Boolean;
var
  CurrentIf: TIfStackEntry;
begin
  if not HasActiveIf then
  begin
    FContext.AddError('ELSE without matching IF', FContext.CurrentToken);
    Result := False;
    Exit;
  end;

  CurrentIf := GetCurrentIf;
  if not Assigned(CurrentIf.ThenNode) then
  begin
    FContext.AddError('ELSE without THEN', FContext.CurrentToken);
    Result := False;
    Exit;
  end;

  if CurrentIf.HasElse then
  begin
    FContext.AddError('Multiple ELSE for same IF', FContext.CurrentToken);
    Result := False;
    Exit;
  end;

  Result := True;
  SetElseForCurrentIf;
end;

function TParserValidationStacks.ValidateThen: Boolean;
begin
  if not HasActiveIf then
  begin
    FContext.AddError('THEN without matching IF', FContext.CurrentToken);
    Result := False;
  end
  else
    Result := True;
end;

// === LOOP VALIDATION ===

procedure TParserValidationStacks.PushLoop(LoopType: TTokenType; LoopNode: TASTNode;
                                          const LoopKeyword, ExpectedEnd: string;
                                          TokenIndex: Integer);
var
  Entry: TLoopStackEntry;
begin
  Entry.LoopType := LoopType;
  Entry.LoopNode := LoopNode;
  Entry.LoopKeyword := LoopKeyword;
  Entry.ExpectedEnd := ExpectedEnd;
  Entry.TokenIndex := TokenIndex;

  if Assigned(LoopNode) and Assigned(LoopNode.Token) then
    Entry.LineNumber := LoopNode.Token.Line
  else
    Entry.LineNumber := 0;

  SetLength(FLoopStack, Length(FLoopStack) + 1);
  FLoopStack[High(FLoopStack)] := Entry;
end;

function TParserValidationStacks.PopLoop(const EndKeyword: string): Boolean;
var
  CurrentLoop: TLoopStackEntry;
begin
  Result := Length(FLoopStack) > 0;

  if Result then
  begin
    CurrentLoop := FLoopStack[High(FLoopStack)];

    // Validate matching keywords
    if UpperCase(EndKeyword) <> UpperCase(CurrentLoop.ExpectedEnd) then
    begin
      FContext.AddError(
        Format('%s without matching %s (found %s)',
               [CurrentLoop.LoopKeyword, CurrentLoop.ExpectedEnd, EndKeyword]),
        FContext.CurrentToken);
      Result := False;
    end;

    SetLength(FLoopStack, Length(FLoopStack) - 1);
  end
  else
  begin
    FContext.AddError(
      Format('%s without matching loop start', [EndKeyword]),
      FContext.CurrentToken);
    Result := False;
  end;
end;

function TParserValidationStacks.GetCurrentLoop: TLoopStackEntry;
begin
  if Length(FLoopStack) > 0 then
    Result := FLoopStack[High(FLoopStack)]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TParserValidationStacks.HasActiveLoop: Boolean;
begin
  Result := Length(FLoopStack) > 0;
end;

function TParserValidationStacks.ValidateLoopEnd(const EndKeyword: string): Boolean;
begin
  Result := PopLoop(EndKeyword);
end;

// === GOSUB/RETURN VALIDATION ===

procedure TParserValidationStacks.PushGosub(GosubNode: TASTNode; TokenIndex: Integer);
var
  Entry: TGosubStackEntry;
begin
  Entry.GosubNode := GosubNode;
  Entry.TokenIndex := TokenIndex;

  if Assigned(GosubNode) and Assigned(GosubNode.Token) then
    Entry.LineNumber := GosubNode.Token.Line
  else
    Entry.LineNumber := 0;

  SetLength(FGosubStack, Length(FGosubStack) + 1);
  FGosubStack[High(FGosubStack)] := Entry;
end;

function TParserValidationStacks.PopGosub: Boolean;
begin
  Result := Length(FGosubStack) > 0;
  if Result then
    SetLength(FGosubStack, Length(FGosubStack) - 1);
end;

function TParserValidationStacks.HasActiveGosub: Boolean;
begin
  Result := Length(FGosubStack) > 0;
end;

function TParserValidationStacks.ValidateReturn: Boolean;
begin
  if not HasActiveGosub then
  begin
    FContext.AddError('RETURN without matching GOSUB', FContext.CurrentToken);
    Result := False;
  end
  else
  begin
    PopGosub;
    Result := True;
  end;
end;

// === BLOCK VALIDATION ===

procedure TParserValidationStacks.PushBlock(BlockType: TTokenType; BlockNode: TASTNode;
                                           const ExpectedEnd: string; TokenIndex: Integer);
var
  Entry: TBlockStackEntry;
begin
  Entry.BlockType := BlockType;
  Entry.BlockNode := BlockNode;
  Entry.ExpectedEnd := ExpectedEnd;
  Entry.TokenIndex := TokenIndex;

  if Assigned(BlockNode) and Assigned(BlockNode.Token) then
    Entry.LineNumber := BlockNode.Token.Line
  else
    Entry.LineNumber := 0;

  SetLength(FBlockStack, Length(FBlockStack) + 1);
  FBlockStack[High(FBlockStack)] := Entry;
end;

function TParserValidationStacks.PopBlock(const EndKeyword: string): Boolean;
var
  CurrentBlock: TBlockStackEntry;
begin
  Result := Length(FBlockStack) > 0;

  if Result then
  begin
    CurrentBlock := FBlockStack[High(FBlockStack)];

    // Validate matching keywords
    if UpperCase(EndKeyword) <> UpperCase(CurrentBlock.ExpectedEnd) then
    begin
      FContext.AddError(
        Format('BEGIN without matching %s (found %s)',
               [CurrentBlock.ExpectedEnd, EndKeyword]),
        FContext.CurrentToken);
      Result := False;
    end;

    SetLength(FBlockStack, Length(FBlockStack) - 1);
  end
  else
  begin
    FContext.AddError(
      Format('%s without matching BEGIN', [EndKeyword]),
      FContext.CurrentToken);
    Result := False;
  end;
end;

function TParserValidationStacks.HasActiveBlock: Boolean;
begin
  Result := Length(FBlockStack) > 0;
end;

function TParserValidationStacks.ValidateBlockEnd(const EndKeyword: string): Boolean;
begin
  Result := PopBlock(EndKeyword);
end;

// === VALIDATION AT END OF PARSING ===

function TParserValidationStacks.ValidateAllClosed: Boolean;
begin
  Result := (Length(FIfStack) = 0) and
            (Length(FLoopStack) = 0) and
            (Length(FGosubStack) = 0) and
            (Length(FBlockStack) = 0);

  if not Result then
    ReportUnclosedConstructs;
end;

procedure TParserValidationStacks.ReportUnclosedConstructs;
var
  i: Integer;
begin
  // Report unclosed IF statements
  for i := 0 to High(FIfStack) do
  begin
    FContext.AddError(
      Format('IF statement at line %d never closed', [FIfStack[i].LineNumber]),
      FIfStack[i].IfNode.Token);
  end;

  // Report unclosed loops
  for i := 0 to High(FLoopStack) do
  begin
    FContext.AddError(
      Format('%s loop at line %d without matching %s',
             [FLoopStack[i].LoopKeyword, FLoopStack[i].LineNumber, FLoopStack[i].ExpectedEnd]),
      FLoopStack[i].LoopNode.Token);
  end;

  // Report unclosed GOSUB calls
  for i := 0 to High(FGosubStack) do
  begin
    FContext.AddError(
      Format('GOSUB at line %d without matching RETURN', [FGosubStack[i].LineNumber]),
      FGosubStack[i].GosubNode.Token);
  end;

  // Report unclosed blocks
  for i := 0 to High(FBlockStack) do
  begin
    FContext.AddError(
      Format('BEGIN block at line %d without matching %s',
             [FBlockStack[i].LineNumber, FBlockStack[i].ExpectedEnd]),
      FBlockStack[i].BlockNode.Token);
  end;
end;

// === UTILITIES ===

procedure TParserValidationStacks.ClearAllStacks;
begin
  SetLength(FIfStack, 0);
  SetLength(FLoopStack, 0);
  SetLength(FGosubStack, 0);
  SetLength(FBlockStack, 0);
end;

function TParserValidationStacks.GetStackInfo: string;
begin
  Result := Format('Validation Stacks - IF: %d, Loop: %d, Gosub: %d, Block: %d',
    [Length(FIfStack), Length(FLoopStack), Length(FGosubStack), Length(FBlockStack)]);
end;

end.
