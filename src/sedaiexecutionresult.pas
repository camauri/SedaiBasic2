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
unit SedaiExecutionResult;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, SedaiExecutorTypes;

type
  TExecutionResult = class
  private
    FData: TExecutionResultData;
    FOutputLines: TStringList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddOutputLine(const Line: string);
    function GetOutput: string;
    procedure Reset;

    property Success: Boolean read FData.Success write FData.Success;
    property ErrorMessage: string read FData.ErrorMessage write FData.ErrorMessage;
    property ExecutionTime: Double read FData.ExecutionTime write FData.ExecutionTime;
    property StatementsExecuted: Integer read FData.StatementsExecuted write FData.StatementsExecuted;
    property OutputLines: TStringList read FOutputLines;
  end;

implementation

{ TExecutionResult }

constructor TExecutionResult.Create;
begin
  inherited Create;
  FData.Success := False;
  FData.ErrorMessage := '';
  FData.ExecutionTime := 0.0;
  FData.StatementsExecuted := 0;
  FOutputLines := TStringList.Create;
end;

destructor TExecutionResult.Destroy;
begin
  FOutputLines.Free;
  inherited Destroy;
end;

procedure TExecutionResult.AddOutputLine(const Line: string);
begin
  FOutputLines.Add(Line);
end;

function TExecutionResult.GetOutput: string;
begin
  Result := FOutputLines.Text;
end;

procedure TExecutionResult.Reset;
begin
  FData.Success := False;
  FData.ErrorMessage := '';
  FData.ExecutionTime := 0.0;
  FData.StatementsExecuted := 0;
  FOutputLines.Clear;
end;

end.
