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
unit SedaiConsoleBehavior;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

type
  { Comportamento della virgola in PRINT }
  TCommaAction = (
    caTabZone,        // TAB alla prossima zona (C64, Atari: 10 col, MSX: 14 col, Spectrum: 16 col)
    caFixedSpaces,    // Numero fisso di spazi
    caNoAction,       // Nessuna azione (solo riposiziona cursore logico)
    caNewLine         // Vai a capo
  );

  { Comportamento del punto e virgola in PRINT }
  TSemicolonAction = (
    saNoSpace,        // Concatena direttamente (C64, Spectrum, Atari)
    saSpaceAfter,     // Spazio dopo ogni elemento
    saSpaceBefore,    // Spazio prima di ogni elemento
    saSpaceBoth       // Spazio prima e dopo
  );

  { Comportamento numeri in PRINT }
  TNumberFormat = (
    nfCommodore,      // Spazio prima se positivo, spazio dopo sempre
    nfMSX,            // Come Commodore
    nfSpectrum,       // Nessuno spazio automatico
    nfAtari,          // Spazio dopo sempre
    nfCustom          // Usa impostazioni personalizzate
  );

  { Comportamento INPUT prompt }
  TInputPromptStyle = (
    ipsCommodore,     // "?" + spazio dopo prompt utente, "? " se nessun prompt
    ipsSpectrum,      // Nessun prompt, solo cursore
    ipsMSX,           // "? " come Commodore
    ipsAtari,         // Nessun "?" automatico
    ipsCustom         // Usa stringa personalizzata
  );

  { Comportamento fine riga }
  TLineEndAction = (
    leaNewLine,       // Vai a capo (default)
    leaStay,          // Resta sulla stessa riga (quando PRINT termina con ;)
    leaScroll         // Scrolla se necessario
  );

  { Comportamento wrap testo }
  TTextWrapMode = (
    twmWrap,          // Wrap automatico alla riga successiva
    twmTruncate,      // Tronca alla fine della riga
    twmScroll         // Scrolla orizzontalmente (raro)
  );

  { Preset per emulare console specifiche }
  TConsolePreset = (
    cpCustom,         // Configurazione personalizzata
    cpCommodore64,    // Commodore 64/128
    cpCommodoreVIC20, // VIC-20 (22 colonne)
    cpCommodorePlus4, // Plus/4
    cpSinclairZX81,   // ZX81 (32 colonne)
    cpSinclairSpectrum, // ZX Spectrum (32 colonne)
    cpMSX,            // MSX standard
    cpMSX2,           // MSX2
    cpAtari800,       // Atari 400/800
    cpAtariST,        // Atari ST
    cpAmstradCPC,     // Amstrad CPC
    cpAppleII,        // Apple II
    cpBBCMicro,       // BBC Micro
    cpTRS80           // TRS-80
  );

  { TConsoleBehavior - Configurazione comportamento console }
  TConsoleBehavior = class
  private
    // Dimensioni schermo
    FScreenCols: Integer;
    FScreenRows: Integer;

    // Comportamento PRINT
    FCommaAction: TCommaAction;
    FCommaTabSize: Integer;           // Dimensione zona TAB per caTabZone
    FCommaSpaces: Integer;            // Numero spazi per caFixedSpaces
    FSemicolonAction: TSemicolonAction;
    FSemicolonSpaces: Integer;        // Numero spazi (se applicabile)

    // Comportamento numeri
    FNumberFormat: TNumberFormat;
    FNumberSpaceBefore: Boolean;      // Spazio prima dei numeri positivi
    FNumberSpaceAfter: Boolean;       // Spazio dopo i numeri
    FNumberSignSpace: Boolean;        // Spazio al posto del + per positivi

    // Comportamento stringhe
    FStringSpaceBefore: Boolean;      // Spazio prima delle stringhe
    FStringSpaceAfter: Boolean;       // Spazio dopo le stringhe

    // Comportamento INPUT
    FInputPromptStyle: TInputPromptStyle;
    FInputPromptString: string;       // Stringa prompt personalizzata (es. "? ")
    FInputPromptOnEmpty: string;      // Prompt quando nessun messaggio utente
    FInputAppendToPrompt: string;     // Caratteri da appendere al prompt utente (es. "? ")

    // Comportamento cursore/wrap
    FTextWrapMode: TTextWrapMode;
    FAutoScroll: Boolean;             // Scroll automatico quando si raggiunge ultima riga
    FPrintNewLineAtEnd: Boolean;      // PRINT senza ; va a capo

    // Comportamento speciale
    FConvertToUpperCase: Boolean;     // Converti input in maiuscolo
    FAllowLowerCase: Boolean;         // Permetti minuscole in input
    FClearLineOnInput: Boolean;       // Pulisci riga prima di INPUT

    // Caratteri speciali
    FNewLineChar: Char;               // Carattere newline (di solito #13)
    FCursorChar: Char;                // Carattere cursore (es. #219 blocco)

  public
    constructor Create;
    destructor Destroy; override;

    { Applica un preset predefinito }
    procedure ApplyPreset(Preset: TConsolePreset);

    { Calcola la prossima posizione TAB per la virgola }
    function GetNextTabPosition(CurrentCol: Integer): Integer;

    { Formatta un numero secondo le regole correnti }
    function FormatNumber(Value: Double): string;

    { Formatta una stringa secondo le regole correnti }
    function FormatString(const S: string): string;

    { Ottiene il prompt per INPUT }
    function GetInputPrompt(const UserPrompt: string): string;

    { Calcola wrap position per una stringa }
    function CalculateWrap(const S: string; StartCol: Integer;
                          out PartOnLine: string; out Remainder: string): Boolean;

    { Clona la configurazione }
    function Clone: TConsoleBehavior;

    { Salva/carica configurazione }
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    // Proprietà - Dimensioni
    property ScreenCols: Integer read FScreenCols write FScreenCols;
    property ScreenRows: Integer read FScreenRows write FScreenRows;

    // Proprietà - PRINT virgola
    property CommaAction: TCommaAction read FCommaAction write FCommaAction;
    property CommaTabSize: Integer read FCommaTabSize write FCommaTabSize;
    property CommaSpaces: Integer read FCommaSpaces write FCommaSpaces;

    // Proprietà - PRINT punto e virgola
    property SemicolonAction: TSemicolonAction read FSemicolonAction write FSemicolonAction;
    property SemicolonSpaces: Integer read FSemicolonSpaces write FSemicolonSpaces;

    // Proprietà - Numeri
    property NumberFormat: TNumberFormat read FNumberFormat write FNumberFormat;
    property NumberSpaceBefore: Boolean read FNumberSpaceBefore write FNumberSpaceBefore;
    property NumberSpaceAfter: Boolean read FNumberSpaceAfter write FNumberSpaceAfter;
    property NumberSignSpace: Boolean read FNumberSignSpace write FNumberSignSpace;

    // Proprietà - Stringhe
    property StringSpaceBefore: Boolean read FStringSpaceBefore write FStringSpaceBefore;
    property StringSpaceAfter: Boolean read FStringSpaceAfter write FStringSpaceAfter;

    // Proprietà - INPUT
    property InputPromptStyle: TInputPromptStyle read FInputPromptStyle write FInputPromptStyle;
    property InputPromptString: string read FInputPromptString write FInputPromptString;
    property InputPromptOnEmpty: string read FInputPromptOnEmpty write FInputPromptOnEmpty;
    property InputAppendToPrompt: string read FInputAppendToPrompt write FInputAppendToPrompt;

    // Proprietà - Wrap/Scroll
    property TextWrapMode: TTextWrapMode read FTextWrapMode write FTextWrapMode;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property PrintNewLineAtEnd: Boolean read FPrintNewLineAtEnd write FPrintNewLineAtEnd;

    // Proprietà - Caratteri
    property ConvertToUpperCase: Boolean read FConvertToUpperCase write FConvertToUpperCase;
    property AllowLowerCase: Boolean read FAllowLowerCase write FAllowLowerCase;
    property ClearLineOnInput: Boolean read FClearLineOnInput write FClearLineOnInput;
    property NewLineChar: Char read FNewLineChar write FNewLineChar;
    property CursorChar: Char read FCursorChar write FCursorChar;
  end;

  { TConsolePresets - Factory per preset predefiniti }
  TConsolePresets = class
  public
    class function CreateCommodore64: TConsoleBehavior;
    class function CreateCommodoreVIC20: TConsoleBehavior;
    class function CreateSinclairSpectrum: TConsoleBehavior;
    class function CreateSinclairZX81: TConsoleBehavior;
    class function CreateMSX: TConsoleBehavior;
    class function CreateAtari800: TConsoleBehavior;
    class function CreateAmstradCPC: TConsoleBehavior;
    class function CreateAppleII: TConsoleBehavior;
    class function CreateBBCMicro: TConsoleBehavior;
    class function CreateTRS80: TConsoleBehavior;
  end;

implementation

{ TConsoleBehavior }

constructor TConsoleBehavior.Create;
begin
  inherited Create;

  // Default: comportamento Commodore 64
  FScreenCols := 40;
  FScreenRows := 25;

  FCommaAction := caTabZone;
  FCommaTabSize := 10;
  FCommaSpaces := 1;

  FSemicolonAction := saNoSpace;
  FSemicolonSpaces := 0;

  FNumberFormat := nfCommodore;
  FNumberSpaceBefore := True;    // Spazio prima se positivo
  FNumberSpaceAfter := True;     // Spazio dopo sempre
  FNumberSignSpace := True;      // Spazio al posto del +

  FStringSpaceBefore := False;
  FStringSpaceAfter := False;

  FInputPromptStyle := ipsCommodore;
  FInputPromptString := '? ';
  FInputPromptOnEmpty := '? ';
  FInputAppendToPrompt := '? ';

  FTextWrapMode := twmWrap;
  FAutoScroll := True;
  FPrintNewLineAtEnd := True;

  FConvertToUpperCase := False;
  FAllowLowerCase := True;
  FClearLineOnInput := False;

  FNewLineChar := #13;
  FCursorChar := #219;  // Blocco pieno
end;

destructor TConsoleBehavior.Destroy;
begin
  inherited Destroy;
end;

procedure TConsoleBehavior.ApplyPreset(Preset: TConsolePreset);
var
  TempBehavior: TConsoleBehavior;
begin
  TempBehavior := nil;

  case Preset of
    cpCommodore64:      TempBehavior := TConsolePresets.CreateCommodore64;
    cpCommodoreVIC20:   TempBehavior := TConsolePresets.CreateCommodoreVIC20;
    cpSinclairSpectrum: TempBehavior := TConsolePresets.CreateSinclairSpectrum;
    cpSinclairZX81:     TempBehavior := TConsolePresets.CreateSinclairZX81;
    cpMSX, cpMSX2:      TempBehavior := TConsolePresets.CreateMSX;
    cpAtari800, cpAtariST: TempBehavior := TConsolePresets.CreateAtari800;
    cpAmstradCPC:       TempBehavior := TConsolePresets.CreateAmstradCPC;
    cpAppleII:          TempBehavior := TConsolePresets.CreateAppleII;
    cpBBCMicro:         TempBehavior := TConsolePresets.CreateBBCMicro;
    cpTRS80:            TempBehavior := TConsolePresets.CreateTRS80;
    cpCustom:           Exit;  // Non fare nulla per custom
  end;

  if Assigned(TempBehavior) then
  begin
    try
      // Copia tutte le proprietà
      FScreenCols := TempBehavior.ScreenCols;
      FScreenRows := TempBehavior.ScreenRows;
      FCommaAction := TempBehavior.CommaAction;
      FCommaTabSize := TempBehavior.CommaTabSize;
      FCommaSpaces := TempBehavior.CommaSpaces;
      FSemicolonAction := TempBehavior.SemicolonAction;
      FSemicolonSpaces := TempBehavior.SemicolonSpaces;
      FNumberFormat := TempBehavior.NumberFormat;
      FNumberSpaceBefore := TempBehavior.NumberSpaceBefore;
      FNumberSpaceAfter := TempBehavior.NumberSpaceAfter;
      FNumberSignSpace := TempBehavior.NumberSignSpace;
      FStringSpaceBefore := TempBehavior.StringSpaceBefore;
      FStringSpaceAfter := TempBehavior.StringSpaceAfter;
      FInputPromptStyle := TempBehavior.InputPromptStyle;
      FInputPromptString := TempBehavior.InputPromptString;
      FInputPromptOnEmpty := TempBehavior.InputPromptOnEmpty;
      FInputAppendToPrompt := TempBehavior.InputAppendToPrompt;
      FTextWrapMode := TempBehavior.TextWrapMode;
      FAutoScroll := TempBehavior.AutoScroll;
      FPrintNewLineAtEnd := TempBehavior.PrintNewLineAtEnd;
      FConvertToUpperCase := TempBehavior.ConvertToUpperCase;
      FAllowLowerCase := TempBehavior.AllowLowerCase;
      FClearLineOnInput := TempBehavior.ClearLineOnInput;
      FNewLineChar := TempBehavior.NewLineChar;
      FCursorChar := TempBehavior.CursorChar;
    finally
      TempBehavior.Free;
    end;
  end;
end;

function TConsoleBehavior.GetNextTabPosition(CurrentCol: Integer): Integer;
begin
  case FCommaAction of
    caTabZone:
      begin
        // Calcola prossima zona TAB
        Result := ((CurrentCol div FCommaTabSize) + 1) * FCommaTabSize;
        // Se oltre la larghezza schermo, vai a capo
        if Result >= FScreenCols then
          Result := 0;
      end;
    caFixedSpaces:
      begin
        Result := CurrentCol + FCommaSpaces;
        if Result >= FScreenCols then
          Result := 0;
      end;
    caNoAction:
      Result := CurrentCol;  // Non cambia
    caNewLine:
      Result := 0;
  else
    Result := CurrentCol;
  end;
end;

function TConsoleBehavior.FormatNumber(Value: Double): string;
var
  Prefix, Suffix: string;
begin
  Prefix := '';
  Suffix := '';

  case FNumberFormat of
    nfCommodore, nfMSX:
      begin
        // Spazio prima se positivo (al posto del segno -)
        if Value >= 0 then
          Prefix := ' '
        else
          Prefix := '';  // Il segno - è già nella rappresentazione

        // Spazio dopo sempre
        Suffix := ' ';
      end;

    nfSpectrum:
      begin
        // Nessuno spazio automatico
        Prefix := '';
        Suffix := '';
      end;

    nfAtari:
      begin
        // Spazio dopo sempre, nessuno prima
        Prefix := '';
        Suffix := ' ';
      end;

    nfCustom:
      begin
        if FNumberSpaceBefore and (Value >= 0) then
          Prefix := ' ';
        if FNumberSpaceAfter then
          Suffix := ' ';
      end;
  end;

  // Formatta il numero
  if Frac(Value) = 0 then
    Result := Prefix + IntToStr(Round(Value)) + Suffix
  else
    Result := Prefix + FloatToStr(Value) + Suffix;
end;

function TConsoleBehavior.FormatString(const S: string): string;
var
  Prefix, Suffix: string;
begin
  Prefix := '';
  Suffix := '';

  if FStringSpaceBefore then
    Prefix := ' ';
  if FStringSpaceAfter then
    Suffix := ' ';

  Result := Prefix + S + Suffix;
end;

function TConsoleBehavior.GetInputPrompt(const UserPrompt: string): string;
begin
  case FInputPromptStyle of
    ipsCommodore:
      begin
        if UserPrompt = '' then
          Result := FInputPromptOnEmpty  // "? "
        else
          Result := UserPrompt + FInputAppendToPrompt;  // "messaggio? "
      end;

    ipsSpectrum:
      begin
        // Spectrum: nessun prompt aggiuntivo
        Result := UserPrompt;
      end;

    ipsMSX:
      begin
        if UserPrompt = '' then
          Result := '? '
        else
          Result := UserPrompt + '? ';
      end;

    ipsAtari:
      begin
        // Atari: stampa solo il messaggio utente, nessun "?"
        Result := UserPrompt;
      end;

    ipsCustom:
      begin
        if UserPrompt = '' then
          Result := FInputPromptOnEmpty
        else
          Result := UserPrompt + FInputAppendToPrompt;
      end;
  else
    Result := UserPrompt;
  end;
end;

function TConsoleBehavior.CalculateWrap(const S: string; StartCol: Integer;
                                        out PartOnLine: string;
                                        out Remainder: string): Boolean;
var
  Available: Integer;
begin
  Available := FScreenCols - StartCol;

  if Length(S) <= Available then
  begin
    // Entra tutto nella riga
    PartOnLine := S;
    Remainder := '';
    Result := False;  // Nessun wrap necessario
  end
  else
  begin
    case FTextWrapMode of
      twmWrap:
        begin
          // Wrap alla riga successiva
          PartOnLine := Copy(S, 1, Available);
          Remainder := Copy(S, Available + 1, Length(S));
          Result := True;
        end;

      twmTruncate:
        begin
          // Tronca
          PartOnLine := Copy(S, 1, Available);
          Remainder := '';
          Result := False;
        end;

      twmScroll:
        begin
          // Per ora tratta come wrap
          PartOnLine := Copy(S, 1, Available);
          Remainder := Copy(S, Available + 1, Length(S));
          Result := True;
        end;
    else
      PartOnLine := S;
      Remainder := '';
      Result := False;
    end;
  end;
end;

function TConsoleBehavior.Clone: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.FScreenCols := FScreenCols;
  Result.FScreenRows := FScreenRows;
  Result.FCommaAction := FCommaAction;
  Result.FCommaTabSize := FCommaTabSize;
  Result.FCommaSpaces := FCommaSpaces;
  Result.FSemicolonAction := FSemicolonAction;
  Result.FSemicolonSpaces := FSemicolonSpaces;
  Result.FNumberFormat := FNumberFormat;
  Result.FNumberSpaceBefore := FNumberSpaceBefore;
  Result.FNumberSpaceAfter := FNumberSpaceAfter;
  Result.FNumberSignSpace := FNumberSignSpace;
  Result.FStringSpaceBefore := FStringSpaceBefore;
  Result.FStringSpaceAfter := FStringSpaceAfter;
  Result.FInputPromptStyle := FInputPromptStyle;
  Result.FInputPromptString := FInputPromptString;
  Result.FInputPromptOnEmpty := FInputPromptOnEmpty;
  Result.FInputAppendToPrompt := FInputAppendToPrompt;
  Result.FTextWrapMode := FTextWrapMode;
  Result.FAutoScroll := FAutoScroll;
  Result.FPrintNewLineAtEnd := FPrintNewLineAtEnd;
  Result.FConvertToUpperCase := FConvertToUpperCase;
  Result.FAllowLowerCase := FAllowLowerCase;
  Result.FClearLineOnInput := FClearLineOnInput;
  Result.FNewLineChar := FNewLineChar;
  Result.FCursorChar := FCursorChar;
end;

procedure TConsoleBehavior.SaveToStream(Stream: TStream);
begin
  // TODO: Implementare serializzazione binaria
end;

procedure TConsoleBehavior.LoadFromStream(Stream: TStream);
begin
  // TODO: Implementare deserializzazione binaria
end;

{ TConsolePresets }

class function TConsolePresets.CreateCommodore64: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 40;
  Result.ScreenRows := 25;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 10;

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfCommodore;
  Result.NumberSpaceBefore := True;
  Result.NumberSpaceAfter := True;
  Result.NumberSignSpace := True;

  Result.InputPromptStyle := ipsCommodore;
  Result.InputPromptOnEmpty := '? ';
  Result.InputAppendToPrompt := '? ';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;

  Result.CursorChar := #219;  // Blocco pieno
end;

class function TConsolePresets.CreateCommodoreVIC20: TConsoleBehavior;
begin
  Result := CreateCommodore64;
  Result.ScreenCols := 22;  // VIC-20 ha 22 colonne
  Result.CommaTabSize := 11;  // Due zone da 11
end;

class function TConsolePresets.CreateSinclairSpectrum: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 32;
  Result.ScreenRows := 24;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 16;  // Due zone da 16 colonne

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfSpectrum;
  Result.NumberSpaceBefore := False;
  Result.NumberSpaceAfter := False;

  Result.InputPromptStyle := ipsSpectrum;
  Result.InputPromptOnEmpty := '';
  Result.InputAppendToPrompt := '';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;

  Result.CursorChar := '_';  // Underscore lampeggiante
end;

class function TConsolePresets.CreateSinclairZX81: TConsoleBehavior;
begin
  Result := CreateSinclairSpectrum;
  Result.ScreenRows := 22;  // ZX81 ha 22 righe effettive
  Result.ConvertToUpperCase := True;  // ZX81 solo maiuscole
  Result.AllowLowerCase := False;
end;

class function TConsolePresets.CreateMSX: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 40;  // Modo SCREEN 0 width 40
  Result.ScreenRows := 24;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 14;  // MSX usa zone da 14

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfMSX;
  Result.NumberSpaceBefore := True;
  Result.NumberSpaceAfter := True;

  Result.InputPromptStyle := ipsMSX;
  Result.InputPromptOnEmpty := '? ';
  Result.InputAppendToPrompt := '? ';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;

  Result.CursorChar := #219;
end;

class function TConsolePresets.CreateAtari800: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 40;
  Result.ScreenRows := 24;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 10;  // Come C64

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfAtari;
  Result.NumberSpaceBefore := False;
  Result.NumberSpaceAfter := True;

  Result.InputPromptStyle := ipsAtari;
  Result.InputPromptOnEmpty := '';  // Atari: nessun "?" automatico
  Result.InputAppendToPrompt := '';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;

  Result.CursorChar := #219;
end;

class function TConsolePresets.CreateAmstradCPC: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 40;  // MODE 1
  Result.ScreenRows := 25;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 13;  // Amstrad usa zone da 13

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfCommodore;  // Simile a Commodore
  Result.NumberSpaceBefore := True;
  Result.NumberSpaceAfter := True;

  Result.InputPromptStyle := ipsCommodore;
  Result.InputPromptOnEmpty := '? ';
  Result.InputAppendToPrompt := '? ';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;

  Result.CursorChar := #219;
end;

class function TConsolePresets.CreateAppleII: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 40;
  Result.ScreenRows := 24;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 16;  // Apple II usa zone da 16

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfCustom;
  Result.NumberSpaceBefore := True;
  Result.NumberSpaceAfter := False;  // Apple II: no spazio dopo

  Result.InputPromptStyle := ipsCustom;
  Result.InputPromptOnEmpty := '?';  // Solo ?, senza spazio
  Result.InputAppendToPrompt := '';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;
  Result.ConvertToUpperCase := True;  // Apple II originale solo maiuscole

  Result.CursorChar := '@';  // Cursore Apple II
end;

class function TConsolePresets.CreateBBCMicro: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 40;  // MODE 7
  Result.ScreenRows := 25;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 10;

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfCustom;
  Result.NumberSpaceBefore := True;
  Result.NumberSpaceAfter := True;

  Result.InputPromptStyle := ipsCustom;
  Result.InputPromptOnEmpty := '?';  // BBC: solo ?
  Result.InputAppendToPrompt := '?';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;

  Result.CursorChar := '_';
end;

class function TConsolePresets.CreateTRS80: TConsoleBehavior;
begin
  Result := TConsoleBehavior.Create;

  Result.ScreenCols := 64;  // Model I/III
  Result.ScreenRows := 16;

  Result.CommaAction := caTabZone;
  Result.CommaTabSize := 16;

  Result.SemicolonAction := saNoSpace;

  Result.NumberFormat := nfCustom;
  Result.NumberSpaceBefore := True;
  Result.NumberSpaceAfter := True;

  Result.InputPromptStyle := ipsCommodore;
  Result.InputPromptOnEmpty := '? ';
  Result.InputAppendToPrompt := '? ';

  Result.TextWrapMode := twmWrap;
  Result.AutoScroll := True;
  Result.ConvertToUpperCase := True;  // TRS-80 solo maiuscole

  Result.CursorChar := #219;
end;

end.
