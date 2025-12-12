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
unit SedaiLogging;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  SysUtils, Classes;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llNone);

  { TLogger - Centralized logging system }
  TLogger = class
  private
    FLogLevel: TLogLevel;
    FLogToFile: Boolean;
    FLogToConsole: Boolean;
    FLogFile: TextFile;
    FLogFileName: string;
    FFileOpened: Boolean;

    procedure WriteToFile(const Msg: string);
    procedure WriteToConsole(const Msg: string);
    function FormatMessage(Level: TLogLevel; const Msg: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    // Configuration
    procedure SetLogLevel(Level: TLogLevel);
    procedure EnableFileLogging(const FileName: string);
    procedure DisableFileLogging;
    procedure EnableConsoleLogging;
    procedure DisableConsoleLogging;

    // Logging methods
    procedure Debug(const Msg: string); overload;
    procedure Debug(const Fmt: string; const Args: array of const); overload;
    procedure Info(const Msg: string); overload;
    procedure Info(const Fmt: string; const Args: array of const); overload;
    procedure Warning(const Msg: string); overload;
    procedure Warning(const Fmt: string; const Args: array of const); overload;
    procedure Error(const Msg: string); overload;
    procedure Error(const Fmt: string; const Args: array of const); overload;

    // Properties
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
  end;

var
  Logger: TLogger;

implementation

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  FLogLevel := llInfo;
  FLogToFile := False;
  FLogToConsole := True;
  FFileOpened := False;
  FLogFileName := '';
end;

destructor TLogger.Destroy;
begin
  DisableFileLogging;
  inherited Destroy;
end;

procedure TLogger.SetLogLevel(Level: TLogLevel);
begin
  FLogLevel := Level;
end;

procedure TLogger.EnableFileLogging(const FileName: string);
begin
  if FFileOpened then
    DisableFileLogging;

  FLogFileName := FileName;
  try
    AssignFile(FLogFile, FLogFileName);
    if FileExists(FLogFileName) then
      Append(FLogFile)
    else
      Rewrite(FLogFile);
    FFileOpened := True;
    FLogToFile := True;
  except
    on E: Exception do
    begin
      FFileOpened := False;
      FLogToFile := False;
      WriteLn('Warning: Could not open log file: ', E.Message);
    end;
  end;
end;

procedure TLogger.DisableFileLogging;
begin
  if FFileOpened then
  begin
    CloseFile(FLogFile);
    FFileOpened := False;
  end;
  FLogToFile := False;
end;

procedure TLogger.EnableConsoleLogging;
begin
  FLogToConsole := True;
end;

procedure TLogger.DisableConsoleLogging;
begin
  FLogToConsole := False;
end;

function TLogger.FormatMessage(Level: TLogLevel; const Msg: string): string;
const
  LevelNames: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARN', 'ERROR', 'NONE');
begin
  Result := Format('[%s] %s: %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    LevelNames[Level],
    Msg
  ]);
end;

procedure TLogger.WriteToFile(const Msg: string);
begin
  if FLogToFile and FFileOpened then
  begin
    try
      WriteLn(FLogFile, Msg);
      Flush(FLogFile);
    except
      // Silently ignore file errors
    end;
  end;
end;

procedure TLogger.WriteToConsole(const Msg: string);
begin
  if FLogToConsole then
    WriteLn(Msg);
end;

procedure TLogger.Debug(const Msg: string);
var
  FormattedMsg: string;
begin
  if FLogLevel > llDebug then Exit;

  FormattedMsg := FormatMessage(llDebug, Msg);
  WriteToConsole(FormattedMsg);
  WriteToFile(FormattedMsg);
end;

procedure TLogger.Debug(const Fmt: string; const Args: array of const);
begin
  Debug(Format(Fmt, Args));
end;

procedure TLogger.Info(const Msg: string);
var
  FormattedMsg: string;
begin
  if FLogLevel > llInfo then Exit;

  FormattedMsg := FormatMessage(llInfo, Msg);
  WriteToConsole(FormattedMsg);
  WriteToFile(FormattedMsg);
end;

procedure TLogger.Info(const Fmt: string; const Args: array of const);
begin
  Info(Format(Fmt, Args));
end;

procedure TLogger.Warning(const Msg: string);
var
  FormattedMsg: string;
begin
  if FLogLevel > llWarning then Exit;

  FormattedMsg := FormatMessage(llWarning, Msg);
  WriteToConsole(FormattedMsg);
  WriteToFile(FormattedMsg);
end;

procedure TLogger.Warning(const Fmt: string; const Args: array of const);
begin
  Warning(Format(Fmt, Args));
end;

procedure TLogger.Error(const Msg: string);
var
  FormattedMsg: string;
begin
  if FLogLevel > llError then Exit;

  FormattedMsg := FormatMessage(llError, Msg);
  WriteToConsole(FormattedMsg);
  WriteToFile(FormattedMsg);
end;

procedure TLogger.Error(const Fmt: string; const Args: array of const);
begin
  Error(Format(Fmt, Args));
end;

initialization
  Logger := TLogger.Create;
  {$IFDEF DEBUG}
  Logger.SetLogLevel(llDebug);
  {$ELSE}
  Logger.SetLogLevel(llWarning);
  {$ENDIF}

finalization
  Logger.Free;

end.
