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
unit SedaiTextOutput;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils, SedaiConsoleBehavior;

type
  { Tipo separatore PRINT }
  TPrintSeparator = (
    psNone,       // Fine PRINT, vai a capo (se configurato)
    psSemicolon,  // ; - concatena secondo configurazione
    psComma       // , - TAB zone secondo configurazione
  );

  { ITextOutput - Interfaccia estesa per output testo configurabile }
  ITextOutput = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

    { Configurazione comportamento }
    function GetBehavior: TConsoleBehavior;
    procedure SetBehavior(ABehavior: TConsoleBehavior);
    property Behavior: TConsoleBehavior read GetBehavior write SetBehavior;

    { Output base }
    procedure WriteChar(Ch: Char);
    procedure WriteString(const S: string);
    procedure WriteLine(const S: string);
    procedure NewLine;
    procedure Clear;

    { Posizionamento diretto }
    procedure SetCursorPos(Col, Row: Integer);
    procedure GetCursorPos(out Col, Row: Integer);
    function GetCursorCol: Integer;
    function GetCursorRow: Integer;

    { Output a posizione specifica }
    procedure WriteCharAt(Col, Row: Integer; Ch: Char);
    procedure WriteStringAt(Col, Row: Integer; const S: string);

    { Output PRINT stile BASIC }
    procedure PrintValue(const Value: Variant; Separator: TPrintSeparator);
    procedure PrintNumber(Value: Double; Separator: TPrintSeparator);
    procedure PrintString(const S: string; Separator: TPrintSeparator);
    procedure PrintNewLine;
    procedure PrintTab(Column: Integer);  // TAB(n) function
    procedure PrintSpc(Count: Integer);   // SPC(n) function

    { Gestione separatori }
    procedure ApplySeparator(Separator: TPrintSeparator);

    { Dimensioni schermo }
    function GetScreenCols: Integer;
    function GetScreenRows: Integer;

    { Scroll }
    procedure ScrollUp(Lines: Integer = 1);
    procedure ScrollDown(Lines: Integer = 1);

    { Cursore visivo }
    procedure ShowCursor;
    procedure HideCursor;
    function IsCursorVisible: Boolean;
  end;

  { TAbstractTextOutput - Implementazione base astratta }
  { NOTE: Using TObject instead of TInterfacedObject because interfaces use CORBA mode (no refcount) }
  TAbstractTextOutput = class(TObject, ITextOutput)
  protected
    FBehavior: TConsoleBehavior;
    FOwnsBehavior: Boolean;
    FCursorCol: Integer;
    FCursorRow: Integer;
    FCursorVisible: Boolean;
    FPendingNewLine: Boolean;  // Per gestire PRINT senza ; alla fine

    { Metodi astratti da implementare nelle sottoclassi }
    procedure DoWriteChar(Col, Row: Integer; Ch: Char); virtual; abstract;
    procedure DoScroll(Direction: Integer; Lines: Integer); virtual; abstract;
    procedure DoClear; virtual; abstract;
    procedure DoShowCursor(Col, Row: Integer); virtual; abstract;
    procedure DoHideCursor(Col, Row: Integer); virtual; abstract;
    function DoGetScreenCols: Integer; virtual; abstract;
    function DoGetScreenRows: Integer; virtual; abstract;

    { Metodi helper }
    procedure AdvanceCursor;
    procedure HandleWrap;
    procedure HandleScroll;

  public
    constructor Create; overload;
    constructor Create(ABehavior: TConsoleBehavior; OwnsBehavior: Boolean = False); overload;
    destructor Destroy; override;

    { ITextOutput implementation }
    function GetBehavior: TConsoleBehavior;
    procedure SetBehavior(ABehavior: TConsoleBehavior);

    procedure WriteChar(Ch: Char); virtual;
    procedure WriteString(const S: string); virtual;
    procedure WriteLine(const S: string); virtual;
    procedure NewLine; virtual;
    procedure Clear; virtual;

    procedure SetCursorPos(Col, Row: Integer); virtual;
    procedure GetCursorPos(out Col, Row: Integer); virtual;
    function GetCursorCol: Integer; virtual;
    function GetCursorRow: Integer; virtual;

    procedure WriteCharAt(Col, Row: Integer; Ch: Char); virtual;
    procedure WriteStringAt(Col, Row: Integer; const S: string); virtual;

    procedure PrintValue(const Value: Variant; Separator: TPrintSeparator); virtual;
    procedure PrintNumber(Value: Double; Separator: TPrintSeparator); virtual;
    procedure PrintString(const S: string; Separator: TPrintSeparator); virtual;
    procedure PrintNewLine; virtual;
    procedure PrintTab(Column: Integer); virtual;
    procedure PrintSpc(Count: Integer); virtual;

    procedure ApplySeparator(Separator: TPrintSeparator); virtual;

    function GetScreenCols: Integer; virtual;
    function GetScreenRows: Integer; virtual;

    procedure ScrollUp(Lines: Integer = 1); virtual;
    procedure ScrollDown(Lines: Integer = 1); virtual;

    procedure ShowCursor; virtual;
    procedure HideCursor; virtual;
    function IsCursorVisible: Boolean; virtual;

    property Behavior: TConsoleBehavior read FBehavior write SetBehavior;
  end;

  { TMemoryTextBuffer - Buffer testo in memoria }
  TMemoryTextBuffer = class(TAbstractTextOutput)
  private
    FBuffer: array of array of Char;
    FScreenCols: Integer;
    FScreenRows: Integer;

  protected
    procedure DoWriteChar(Col, Row: Integer; Ch: Char); override;
    procedure DoScroll(Direction: Integer; Lines: Integer); override;
    procedure DoClear; override;
    procedure DoShowCursor(Col, Row: Integer); override;
    procedure DoHideCursor(Col, Row: Integer); override;
    function DoGetScreenCols: Integer; override;
    function DoGetScreenRows: Integer; override;

  public
    constructor Create(Cols, Rows: Integer); overload;
    constructor Create(Cols, Rows: Integer; ABehavior: TConsoleBehavior); overload;
    destructor Destroy; override;

    { Accesso diretto al buffer }
    function GetChar(Col, Row: Integer): Char;
    function GetLine(Row: Integer): string;
    function GetAllText: string;

    { Resize }
    procedure Resize(NewCols, NewRows: Integer);
  end;

  { TStdTextOutput - Output su console standard (cmd/bash) }
  TStdTextOutput = class(TAbstractTextOutput)
  private
    FUseANSI: Boolean;
    FScreenCols: Integer;
    FScreenRows: Integer;
    {$IFDEF WINDOWS}
    FConsoleHandle: THandle;
    {$ENDIF}

    procedure DetectCapabilities;
    procedure WriteANSI(const S: string);

  protected
    procedure DoWriteChar(Col, Row: Integer; Ch: Char); override;
    procedure DoScroll(Direction: Integer; Lines: Integer); override;
    procedure DoClear; override;
    procedure DoShowCursor(Col, Row: Integer); override;
    procedure DoHideCursor(Col, Row: Integer); override;
    function DoGetScreenCols: Integer; override;
    function DoGetScreenRows: Integer; override;

  public
    constructor Create; overload;
    constructor Create(ABehavior: TConsoleBehavior); overload;
    destructor Destroy; override;

    property UseANSI: Boolean read FUseANSI write FUseANSI;
  end;

implementation

uses
  Variants
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  ;

{ TAbstractTextOutput }

constructor TAbstractTextOutput.Create;
begin
  inherited Create;
  FBehavior := TConsoleBehavior.Create;
  FOwnsBehavior := True;
  FCursorCol := 0;
  FCursorRow := 0;
  FCursorVisible := True;
  FPendingNewLine := False;
end;

constructor TAbstractTextOutput.Create(ABehavior: TConsoleBehavior; OwnsBehavior: Boolean);
begin
  inherited Create;
  if Assigned(ABehavior) then
  begin
    FBehavior := ABehavior;
    FOwnsBehavior := OwnsBehavior;
  end
  else
  begin
    FBehavior := TConsoleBehavior.Create;
    FOwnsBehavior := True;
  end;
  FCursorCol := 0;
  FCursorRow := 0;
  FCursorVisible := True;
  FPendingNewLine := False;
end;

destructor TAbstractTextOutput.Destroy;
begin
  if FOwnsBehavior and Assigned(FBehavior) then
    FBehavior.Free;
  inherited Destroy;
end;

function TAbstractTextOutput.GetBehavior: TConsoleBehavior;
begin
  Result := FBehavior;
end;

procedure TAbstractTextOutput.SetBehavior(ABehavior: TConsoleBehavior);
begin
  if FOwnsBehavior and Assigned(FBehavior) then
    FBehavior.Free;

  FBehavior := ABehavior;
  FOwnsBehavior := False;
end;

procedure TAbstractTextOutput.AdvanceCursor;
begin
  Inc(FCursorCol);
  if FCursorCol >= DoGetScreenCols then
    HandleWrap;
end;

procedure TAbstractTextOutput.HandleWrap;
begin
  case FBehavior.TextWrapMode of
    twmWrap:
      begin
        FCursorCol := 0;
        Inc(FCursorRow);
        if FCursorRow >= DoGetScreenRows then
          HandleScroll;
      end;
    twmTruncate:
      begin
        // Resta alla fine della riga
        FCursorCol := DoGetScreenCols - 1;
      end;
    twmScroll:
      begin
        // Scroll orizzontale (non comune, tratto come wrap)
        FCursorCol := 0;
        Inc(FCursorRow);
        if FCursorRow >= DoGetScreenRows then
          HandleScroll;
      end;
  end;
end;

procedure TAbstractTextOutput.HandleScroll;
begin
  if FBehavior.AutoScroll then
  begin
    DoScroll(-1, 1);  // Scroll up
    FCursorRow := DoGetScreenRows - 1;
  end
  else
  begin
    // Resta sull'ultima riga
    FCursorRow := DoGetScreenRows - 1;
  end;
end;

procedure TAbstractTextOutput.WriteChar(Ch: Char);
begin
  if Ch = FBehavior.NewLineChar then
  begin
    NewLine;
    Exit;
  end;

  DoWriteChar(FCursorCol, FCursorRow, Ch);
  AdvanceCursor;
end;

procedure TAbstractTextOutput.WriteString(const S: string);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    WriteChar(S[i]);
end;

procedure TAbstractTextOutput.WriteLine(const S: string);
begin
  WriteString(S);
  NewLine;
end;

procedure TAbstractTextOutput.NewLine;
begin
  FCursorCol := 0;
  Inc(FCursorRow);
  if FCursorRow >= DoGetScreenRows then
    HandleScroll;
end;

procedure TAbstractTextOutput.Clear;
begin
  DoClear;
  FCursorCol := 0;
  FCursorRow := 0;
end;

procedure TAbstractTextOutput.SetCursorPos(Col, Row: Integer);
begin
  FCursorCol := Col;
  FCursorRow := Row;

  // Clamp to valid range
  if FCursorCol < 0 then FCursorCol := 0;
  if FCursorCol >= DoGetScreenCols then FCursorCol := DoGetScreenCols - 1;
  if FCursorRow < 0 then FCursorRow := 0;
  if FCursorRow >= DoGetScreenRows then FCursorRow := DoGetScreenRows - 1;
end;

procedure TAbstractTextOutput.GetCursorPos(out Col, Row: Integer);
begin
  Col := FCursorCol;
  Row := FCursorRow;
end;

function TAbstractTextOutput.GetCursorCol: Integer;
begin
  Result := FCursorCol;
end;

function TAbstractTextOutput.GetCursorRow: Integer;
begin
  Result := FCursorRow;
end;

procedure TAbstractTextOutput.WriteCharAt(Col, Row: Integer; Ch: Char);
var
  SaveCol, SaveRow: Integer;
begin
  SaveCol := FCursorCol;
  SaveRow := FCursorRow;

  FCursorCol := Col;
  FCursorRow := Row;
  DoWriteChar(Col, Row, Ch);

  FCursorCol := SaveCol;
  FCursorRow := SaveRow;
end;

procedure TAbstractTextOutput.WriteStringAt(Col, Row: Integer; const S: string);
var
  i, CurrentCol: Integer;
  PartOnLine, Remainder: string;
  CurrentRow: Integer;
begin
  CurrentCol := Col;
  CurrentRow := Row;

  // Usa il sistema di wrap del behavior
  if FBehavior.CalculateWrap(S, CurrentCol, PartOnLine, Remainder) then
  begin
    // Prima parte sulla riga corrente
    for i := 1 to Length(PartOnLine) do
    begin
      DoWriteChar(CurrentCol, CurrentRow, PartOnLine[i]);
      Inc(CurrentCol);
    end;

    // Remainder sulle righe successive
    while Remainder <> '' do
    begin
      Inc(CurrentRow);
      if CurrentRow >= DoGetScreenRows then
      begin
        if FBehavior.AutoScroll then
        begin
          DoScroll(-1, 1);
          CurrentRow := DoGetScreenRows - 1;
        end
        else
          Break;  // Non possiamo continuare
      end;

      CurrentCol := 0;
      if FBehavior.CalculateWrap(Remainder, 0, PartOnLine, Remainder) then
      begin
        for i := 1 to Length(PartOnLine) do
        begin
          DoWriteChar(CurrentCol, CurrentRow, PartOnLine[i]);
          Inc(CurrentCol);
        end;
      end
      else
      begin
        // Ultima parte
        for i := 1 to Length(PartOnLine) do
        begin
          DoWriteChar(CurrentCol, CurrentRow, PartOnLine[i]);
          Inc(CurrentCol);
        end;
        Break;
      end;
    end;
  end
  else
  begin
    // Tutto entra nella riga
    for i := 1 to Length(S) do
    begin
      DoWriteChar(CurrentCol, CurrentRow, S[i]);
      Inc(CurrentCol);
    end;
  end;
end;

procedure TAbstractTextOutput.PrintValue(const Value: Variant; Separator: TPrintSeparator);
begin
  case VarType(Value) and varTypeMask of
    varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
      PrintNumber(Value, Separator);
    varSingle, varDouble, varCurrency:
      PrintNumber(Value, Separator);
    varString, varOleStr, varUString:
      PrintString(Value, Separator);
  else
    PrintString(VarToStr(Value), Separator);
  end;
end;

procedure TAbstractTextOutput.PrintNumber(Value: Double; Separator: TPrintSeparator);
var
  Formatted: string;
begin
  Formatted := FBehavior.FormatNumber(Value);
  WriteString(Formatted);
  ApplySeparator(Separator);
end;

procedure TAbstractTextOutput.PrintString(const S: string; Separator: TPrintSeparator);
var
  Formatted: string;
begin
  Formatted := FBehavior.FormatString(S);
  WriteString(Formatted);
  ApplySeparator(Separator);
end;

procedure TAbstractTextOutput.PrintNewLine;
begin
  NewLine;
end;

procedure TAbstractTextOutput.PrintTab(Column: Integer);
var
  SpacesToAdd: Integer;
begin
  // TAB(n) in BASIC: vai alla colonna n (1-based solitamente)
  // Se già oltre, potrebbe andare a capo (dipende dal dialetto)

  if Column < 1 then
    Column := 1;

  // Converti a 0-based
  Dec(Column);

  if Column < DoGetScreenCols then
  begin
    if FCursorCol < Column then
    begin
      // Aggiungi spazi per arrivare alla colonna
      SpacesToAdd := Column - FCursorCol;
      while SpacesToAdd > 0 do
      begin
        WriteChar(' ');
        Dec(SpacesToAdd);
      end;
    end
    else if FCursorCol > Column then
    begin
      // Già oltre: vai a capo e poi alla colonna
      NewLine;
      SpacesToAdd := Column;
      while SpacesToAdd > 0 do
      begin
        WriteChar(' ');
        Dec(SpacesToAdd);
      end;
    end;
    // Se FCursorCol = Column, non fare nulla
  end;
end;

procedure TAbstractTextOutput.PrintSpc(Count: Integer);
var
  i: Integer;
begin
  for i := 1 to Count do
    WriteChar(' ');
end;

procedure TAbstractTextOutput.ApplySeparator(Separator: TPrintSeparator);
var
  NextCol, i: Integer;
begin
  case Separator of
    psNone:
      begin
        // Fine del PRINT statement
        if FBehavior.PrintNewLineAtEnd then
          NewLine;
      end;

    psSemicolon:
      begin
        case FBehavior.SemicolonAction of
          saNoSpace:
            ; // Non fare nulla
          saSpaceAfter:
            WriteChar(' ');
          saSpaceBefore:
            ; // Lo spazio prima viene gestito dal prossimo elemento
          saSpaceBoth:
            WriteChar(' ');
        end;
      end;

    psComma:
      begin
        case FBehavior.CommaAction of
          caTabZone:
            begin
              NextCol := FBehavior.GetNextTabPosition(FCursorCol);
              if NextCol = 0 then
              begin
                // Wrap alla riga successiva
                NewLine;
              end
              else
              begin
                // Spazi fino alla prossima zona
                while FCursorCol < NextCol do
                  WriteChar(' ');
              end;
            end;

          caFixedSpaces:
            begin
              for i := 1 to FBehavior.CommaSpaces do
                WriteChar(' ');
            end;

          caNoAction:
            begin
              // Solo riposiziona il cursore logico senza scrivere
              NextCol := FBehavior.GetNextTabPosition(FCursorCol);
              if NextCol = 0 then
                NewLine
              else
                FCursorCol := NextCol;
            end;

          caNewLine:
            NewLine;
        end;
      end;
  end;
end;

function TAbstractTextOutput.GetScreenCols: Integer;
begin
  Result := DoGetScreenCols;
end;

function TAbstractTextOutput.GetScreenRows: Integer;
begin
  Result := DoGetScreenRows;
end;

procedure TAbstractTextOutput.ScrollUp(Lines: Integer);
begin
  DoScroll(-1, Lines);
end;

procedure TAbstractTextOutput.ScrollDown(Lines: Integer);
begin
  DoScroll(1, Lines);
end;

procedure TAbstractTextOutput.ShowCursor;
begin
  if not FCursorVisible then
  begin
    FCursorVisible := True;
    DoShowCursor(FCursorCol, FCursorRow);
  end;
end;

procedure TAbstractTextOutput.HideCursor;
begin
  if FCursorVisible then
  begin
    FCursorVisible := False;
    DoHideCursor(FCursorCol, FCursorRow);
  end;
end;

function TAbstractTextOutput.IsCursorVisible: Boolean;
begin
  Result := FCursorVisible;
end;

{ TMemoryTextBuffer }

constructor TMemoryTextBuffer.Create(Cols, Rows: Integer);
begin
  inherited Create;
  FScreenCols := Cols;
  FScreenRows := Rows;
  FBehavior.ScreenCols := Cols;
  FBehavior.ScreenRows := Rows;
  Resize(Cols, Rows);
end;

constructor TMemoryTextBuffer.Create(Cols, Rows: Integer; ABehavior: TConsoleBehavior);
begin
  inherited Create(ABehavior, False);
  FScreenCols := Cols;
  FScreenRows := Rows;
  if Assigned(FBehavior) then
  begin
    FBehavior.ScreenCols := Cols;
    FBehavior.ScreenRows := Rows;
  end;
  Resize(Cols, Rows);
end;

destructor TMemoryTextBuffer.Destroy;
begin
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

procedure TMemoryTextBuffer.Resize(NewCols, NewRows: Integer);
var
  Row, Col: Integer;
begin
  SetLength(FBuffer, NewRows, NewCols);

  // Inizializza con spazi
  for Row := 0 to NewRows - 1 do
    for Col := 0 to NewCols - 1 do
      FBuffer[Row][Col] := ' ';

  FScreenCols := NewCols;
  FScreenRows := NewRows;

  if Assigned(FBehavior) then
  begin
    FBehavior.ScreenCols := NewCols;
    FBehavior.ScreenRows := NewRows;
  end;

  // Aggiusta cursore se fuori range
  if FCursorCol >= NewCols then FCursorCol := NewCols - 1;
  if FCursorRow >= NewRows then FCursorRow := NewRows - 1;
end;

procedure TMemoryTextBuffer.DoWriteChar(Col, Row: Integer; Ch: Char);
begin
  if (Col >= 0) and (Col < FScreenCols) and
     (Row >= 0) and (Row < FScreenRows) then
    FBuffer[Row][Col] := Ch;
end;

procedure TMemoryTextBuffer.DoScroll(Direction: Integer; Lines: Integer);
var
  Row, Col, SrcRow: Integer;
begin
  if Lines <= 0 then Exit;

  if Direction < 0 then
  begin
    // Scroll up: le righe salgono
    for Row := 0 to FScreenRows - Lines - 1 do
    begin
      SrcRow := Row + Lines;
      for Col := 0 to FScreenCols - 1 do
        FBuffer[Row][Col] := FBuffer[SrcRow][Col];
    end;

    // Pulisci le righe in basso
    for Row := FScreenRows - Lines to FScreenRows - 1 do
      for Col := 0 to FScreenCols - 1 do
        FBuffer[Row][Col] := ' ';
  end
  else
  begin
    // Scroll down: le righe scendono
    for Row := FScreenRows - 1 downto Lines do
    begin
      SrcRow := Row - Lines;
      for Col := 0 to FScreenCols - 1 do
        FBuffer[Row][Col] := FBuffer[SrcRow][Col];
    end;

    // Pulisci le righe in alto
    for Row := 0 to Lines - 1 do
      for Col := 0 to FScreenCols - 1 do
        FBuffer[Row][Col] := ' ';
  end;
end;

procedure TMemoryTextBuffer.DoClear;
var
  Row, Col: Integer;
begin
  for Row := 0 to FScreenRows - 1 do
    for Col := 0 to FScreenCols - 1 do
      FBuffer[Row][Col] := ' ';
end;

procedure TMemoryTextBuffer.DoShowCursor(Col, Row: Integer);
begin
  // Il buffer in memoria non ha cursore visivo
end;

procedure TMemoryTextBuffer.DoHideCursor(Col, Row: Integer);
begin
  // Il buffer in memoria non ha cursore visivo
end;

function TMemoryTextBuffer.DoGetScreenCols: Integer;
begin
  Result := FScreenCols;
end;

function TMemoryTextBuffer.DoGetScreenRows: Integer;
begin
  Result := FScreenRows;
end;

function TMemoryTextBuffer.GetChar(Col, Row: Integer): Char;
begin
  if (Col >= 0) and (Col < FScreenCols) and
     (Row >= 0) and (Row < FScreenRows) then
    Result := FBuffer[Row][Col]
  else
    Result := ' ';
end;

function TMemoryTextBuffer.GetLine(Row: Integer): string;
var
  Col: Integer;
begin
  Result := '';
  if (Row >= 0) and (Row < FScreenRows) then
  begin
    SetLength(Result, FScreenCols);
    for Col := 0 to FScreenCols - 1 do
      Result[Col + 1] := FBuffer[Row][Col];
  end;
end;

function TMemoryTextBuffer.GetAllText: string;
var
  Row: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    for Row := 0 to FScreenRows - 1 do
    begin
      SB.Append(GetLine(Row));
      if Row < FScreenRows - 1 then
        SB.AppendLine;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TStdTextOutput }

constructor TStdTextOutput.Create;
begin
  inherited Create;
  DetectCapabilities;
end;

constructor TStdTextOutput.Create(ABehavior: TConsoleBehavior);
begin
  inherited Create(ABehavior, False);
  DetectCapabilities;
end;

destructor TStdTextOutput.Destroy;
begin
  inherited Destroy;
end;

procedure TStdTextOutput.DetectCapabilities;
{$IFDEF WINDOWS}
var
  ConsoleInfo: TConsoleScreenBufferInfo;
  Mode: DWORD;
{$ENDIF}
begin
  FUseANSI := False;
  FScreenCols := 80;
  FScreenRows := 25;

  {$IFDEF WINDOWS}
  FConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  if FConsoleHandle <> INVALID_HANDLE_VALUE then
  begin
    // Ottieni dimensioni console
    if GetConsoleScreenBufferInfo(FConsoleHandle, @ConsoleInfo) then
    begin
      FScreenCols := ConsoleInfo.srWindow.Right - ConsoleInfo.srWindow.Left + 1;
      FScreenRows := ConsoleInfo.srWindow.Bottom - ConsoleInfo.srWindow.Top + 1;
    end;

    // Verifica supporto ANSI (Windows 10+)
    if GetConsoleMode(FConsoleHandle, @Mode) then
    begin
      // ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004
      if (Mode and $0004) <> 0 then
        FUseANSI := True
      else
      begin
        // Prova ad abilitare ANSI
        if SetConsoleMode(FConsoleHandle, Mode or $0004) then
          FUseANSI := True;
      end;
    end;
  end;
  {$ELSE}
  // Unix: assume sempre supporto ANSI
  FUseANSI := True;
  // TODO: leggere COLUMNS e LINES da environment o ioctl
  {$ENDIF}

  if Assigned(FBehavior) then
  begin
    FBehavior.ScreenCols := FScreenCols;
    FBehavior.ScreenRows := FScreenRows;
  end;
end;

procedure TStdTextOutput.WriteANSI(const S: string);
begin
  Write(S);
end;

procedure TStdTextOutput.DoWriteChar(Col, Row: Integer; Ch: Char);
begin
  if FUseANSI then
  begin
    // Escape sequence per posizionamento: ESC[row;colH
    WriteANSI(Format(#27'[%d;%dH%s', [Row + 1, Col + 1, Ch]));
  end
  else
  begin
    {$IFDEF WINDOWS}
    // Usa API Windows diretta
    var Coord: TCoord;
    var Written: DWORD;
    Coord.X := Col;
    Coord.Y := Row;
    SetConsoleCursorPosition(FConsoleHandle, Coord);
    WriteConsole(FConsoleHandle, @Ch, 1, @Written, nil);
    {$ELSE}
    // Fallback: scrivi direttamente
    Write(Ch);
    {$ENDIF}
  end;
end;

procedure TStdTextOutput.DoScroll(Direction: Integer; Lines: Integer);
begin
  if FUseANSI then
  begin
    if Direction < 0 then
      // Scroll up
      WriteANSI(Format(#27'[%dS', [Lines]))
    else
      // Scroll down
      WriteANSI(Format(#27'[%dT', [Lines]));
  end
  else
  begin
    // Fallback: stampa newline
    WriteLn;
  end;
end;

procedure TStdTextOutput.DoClear;
begin
  if FUseANSI then
  begin
    WriteANSI(#27'[2J');   // Clear screen
    WriteANSI(#27'[H');    // Home cursor
  end
  else
  begin
    {$IFDEF WINDOWS}
    var ConsoleInfo: TConsoleScreenBufferInfo;
    var Coord: TCoord;
    var Written: DWORD;
    var Size: DWORD;

    if GetConsoleScreenBufferInfo(FConsoleHandle, @ConsoleInfo) then
    begin
      Size := ConsoleInfo.dwSize.X * ConsoleInfo.dwSize.Y;
      Coord.X := 0;
      Coord.Y := 0;
      FillConsoleOutputCharacter(FConsoleHandle, ' ', Size, Coord, @Written);
      SetConsoleCursorPosition(FConsoleHandle, Coord);
    end;
    {$ENDIF}
  end;
end;

procedure TStdTextOutput.DoShowCursor(Col, Row: Integer);
begin
  if FUseANSI then
    WriteANSI(#27'[?25h');  // Show cursor
end;

procedure TStdTextOutput.DoHideCursor(Col, Row: Integer);
begin
  if FUseANSI then
    WriteANSI(#27'[?25l');  // Hide cursor
end;

function TStdTextOutput.DoGetScreenCols: Integer;
begin
  Result := FScreenCols;
end;

function TStdTextOutput.DoGetScreenRows: Integer;
begin
  Result := FScreenRows;
end;

end.
