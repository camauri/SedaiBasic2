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
  { Behaviour of the comma in PRINT }
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
    nfCustom,         // Usa impostazioni personalizzate
    // FreeBASIC (-lang fb): leading space for the sign, and NO trailing space. The trailing space is a
    // QB-dialect trait -- the FB manual's Print page says so outright ("In the -lang qb dialect, an extra
    // space is printed after numbers") -- so it belongs to CLASSIC (v7), not to MODERN.
    nfFreeBASIC,
    // FreeBASIC's -lang qb: the trailing space is back, but ONLY after an INTEGER. A Single or a Double
    // keeps the leading sign pad and nothing after it - measured against fbc, which prints "E 24 !" for
    // an Integer 24 and "E 24!" for a Single 24, same digits, same statement. It is the DECLARED TYPE
    // that decides, so the split falls exactly where FormatInt and FormatNumber already part company.
    nfQB
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
    twmTruncate,      // truncate at the end of the line
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
    { How many significant digits PRINT shows for a float. 16 for a Double is
      the DIALECT's display precision, and 7 for a Single is what hides its
      representation error (a 24-bit mantissa is worth about 7.2 decimal
      digits). ⭐ The digit COUNT is a display choice; the ROUNDING is not - at
      every setting the digits are correctly rounded from the exact binary
      value, so raising this shows more of the same number rather than a
      differently-rounded one. 17 makes every distinct double print distinctly
      (the round-trip guarantee); beyond that the expansion is exact and
      terminates, so the extra digits are the true ones, not noise. }
    FFloatDigits: Integer;
    FSingleDigits: Integer;
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

    // ⚠️ I metodi vanno DOPO tutti i campi: in Pascal un campo non può seguire
    // una dichiarazione di metodo nella stessa sezione di visibilità.
    procedure SetFloatDigits(V: Integer);
    procedure SetSingleDigits(V: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    { Applica un preset predefinito }
    procedure ApplyPreset(Preset: TConsolePreset);

    { Calcola la prossima posizione TAB per la virgola }
    function GetNextTabPosition(CurrentCol: Integer): Integer;

    { Formatta un numero secondo le regole correnti }
    // AsSingle: format with a SINGLE's precision (7 significant digits) rather than a DOUBLE's 16.
    function FormatNumber(Value: Double; AsSingle: Boolean = False): string;

    { Formatta un intero senza segno a 64 bit (B1.5: UInteger/ULongInt), valore sempre >= 0 }
    function FormatUInt(Value: QWord): string;

    { Formatta un intero con segno a 64 bit in modo esatto. Come FormatNumber ma senza il passaggio
      per Double (che perde precisione oltre 2^53): un Integer/LongInt grande deve stampare esatto. }
    function FormatInt(Value: Int64): string;

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
    { "OPTION DIGITS n". Clamped to 1..MAX_FLOAT_DIGITS on the way in, because a
      count of zero has no meaning and an unbounded one would only pad zeros. }
    property FloatDigits: Integer read FFloatDigits write SetFloatDigits;
    property SingleDigits: Integer read FSingleDigits write SetSingleDigits;
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

{ The correctly rounded double of a decimal string - VAL()'s tail end. Exported
  because the VM's ParseLeadingFloat is what calls it, and because it is the
  mirror of the exact digit generator that lives beside it: the two have to be
  read together. }
function ExactStrToDouble(const S: string): Double;

implementation

uses Math;

const
  { ⭐ NOT a truncation, and worth being precise about because it looks like one:
    a double's decimal expansion is FINITE. The value is M x 2^E, so for E >= 0
    it is an integer (at most 309 digits) and for E < 0 it is
    M x 5^(-E) / 10^(-E), which TERMINATES after exactly -E fractional digits.
    The widest case is the smallest subnormal, 2^-1074, whose exact value has
    751 significant digits - MEASURED against the exact expansion, not assumed.
    M x 5^1074 bounds it at 767, so at this setting every double prints its
    mathematically exact value and asking for more cannot show anything: there is
    nothing past the end of an expansion that ends. }
  MAX_FLOAT_DIGITS = 767;


function ExactRoundedDigits(Value: Double; SIGDIGITS: Integer; out Ex: Integer): string;
{ The first SIGDIGITS significant decimal digits of |Value|, CORRECTLY ROUNDED
  (round-half-even) from the EXACT binary value, plus the decimal exponent of the
  leading digit. No floating point is used anywhere below.

  ⭐ WHY THIS EXISTS, because the obvious way looked fine for years and was not.
  The digits used to come from FPC's Str, which yields 17 of them, and the caller
  then rounded those 17 down to 16. That is a DOUBLE ROUNDING, and it disagrees
  with a single correct rounding on 4.75% of doubles - measured, not estimated:
  the 17-digit intermediate lands on ...5 about a tenth of the time, and in half
  of those the exact value was strictly below the halfway point, so the second
  rounding pushes it the wrong way. The textbook case is 1e-283, whose exact
  value is 0.999999999999999946852...e-283: the 17th digit is 4, so correct
  rounding gives 9.999999999999999e-284, while rounding to 17 first turns ...946
  into ...95 and then carries through sixteen nines to print "1e-283".
  ⛔ fbc does the double rounding and is wrong there; IEEE 754-2019 §5.12.2 asks
  for the correctly rounded conversion, and this project's VM is required to be
  deterministic and precise. So we deliberately DIFFER FROM fbc on those values -
  see job/docs/PIANO_FLOAT_PRINT.md before "fixing" a sweep DIFF back.
  ⚠️ FPC's own Str was not even faithful to fbc: it mis-rounds one value in
  20706 in a THIRD way, agreeing with neither.

  THE METHOD. A double is M x 2^E with M a 53-bit integer, so
      E >= 0 :  the value is the integer M x 2^E
      E <  0 :  the value is M x 5^(-E) / 10^(-E)
  Either way the exact decimal digits are those of an INTEGER built by repeated
  multiplication - no division, no big-number division, no reciprocals. The
  widest case is M x 5^1074, which is 767 digits. }
const
  MAXDIG = 1100;              // 767 is the true worst case; the slack is free
  { The factors are applied in CHUNKS, not one at a time: a digit is at most 9,
    so 9*5^13 plus the carry still fits an Int64 comfortably, and thirteen
    multiplications collapse into one pass. 1074 passes over the buffer become
    83, which is what keeps this off the profile of an ordinary PRINT. }
  P5CHUNK = 13;
  P2CHUNK = 30;
var
  Buf: array[0..MAXDIG - 1] of Byte;   // little-endian decimal digits
  Len, FracDigits, i, k, Keep, First, D: Integer;
  Bits, M, E, T, Carry, Mul: Int64;
  RoundUp, AnyBelow: Boolean;

  procedure MulBy(F: Int64);
  var
    j: Integer;
  begin
    Carry := 0;
    for j := 0 to Len - 1 do
    begin
      T := Int64(Buf[j]) * F + Carry;
      Buf[j] := Byte(T mod 10);
      Carry := T div 10;
    end;
    while Carry > 0 do
    begin
      Buf[Len] := Byte(Carry mod 10);
      Carry := Carry div 10;
      Inc(Len);
    end;
  end;

begin
  Bits := PInt64(@Value)^;
  M := Bits and $000FFFFFFFFFFFFF;
  E := (Bits shr 52) and $7FF;
  if E = 0 then
    E := -1074                       // subnormal: no implicit leading bit
  else
  begin
    M := M or (Int64(1) shl 52);
    E := E - 1075;
  end;
  if M = 0 then
  begin
    Ex := 0;
    Exit(StringOfChar('0', SIGDIGITS));
  end;

  Len := 0;
  while M > 0 do
  begin
    Buf[Len] := Byte(M mod 10);
    M := M div 10;
    Inc(Len);
  end;

  if E >= 0 then
  begin
    FracDigits := 0;
    i := Integer(E);
    while i > 0 do
    begin
      k := i; if k > P2CHUNK then k := P2CHUNK;
      Mul := 1; for D := 1 to k do Mul := Mul * 2;
      MulBy(Mul);
      Dec(i, k);
    end;
  end
  else
  begin
    FracDigits := Integer(-E);
    i := FracDigits;
    while i > 0 do
    begin
      k := i; if k > P5CHUNK then k := P5CHUNK;
      Mul := 1; for D := 1 to k do Mul := Mul * 5;
      MulBy(Mul);
      Dec(i, k);
    end;
  end;

  // The value is Buf (as an integer) x 10^-FracDigits, and Buf's top digit is
  // nonzero, so the leading significant digit sits at index Len-1.
  Ex := (Len - 1) - FracDigits;

  if Len <= SIGDIGITS then
  begin
    // Fewer exact digits than asked for: pad, and there is nothing to round.
    SetLength(Result, SIGDIGITS);
    for i := 1 to SIGDIGITS do
      if i <= Len then Result[i] := Chr(Ord('0') + Buf[Len - i])
                  else Result[i] := '0';
    Exit;
  end;

  Keep := SIGDIGITS;
  First := Len - Keep;                     // index of the first DROPPED digit
  SetLength(Result, Keep);
  for i := 1 to Keep do
    Result[i] := Chr(Ord('0') + Buf[Len - i]);

  // Round half to EVEN on the exact remainder - which is the whole point: the
  // decision looks at EVERY dropped digit, not just the first one. Looking only
  // at the first is what a double rounding does, and it is the 4.75%.
  D := Buf[First - 1];                     // the first dropped digit
  if D > 5 then
    RoundUp := True
  else if D < 5 then
    RoundUp := False
  else
  begin
    AnyBelow := False;
    for i := 0 to First - 2 do
      if Buf[i] <> 0 then begin AnyBelow := True; Break; end;
    if AnyBelow then RoundUp := True
                else RoundUp := Odd(Ord(Result[Keep]) - Ord('0'));   // exact tie
  end;

  if RoundUp then
  begin
    i := Keep;
    while i >= 1 do
    begin
      if Result[i] < '9' then begin Inc(Result[i]); Break; end;
      Result[i] := '0';
      Dec(i);
    end;
    if i = 0 then
    begin
      // the carry ran off the front: 99..9 became 10..0, one decade up
      Result := '1' + Copy(Result, 1, Keep - 1);
      Inc(Ex);
    end;
  end;
end;

function ExactStrToDouble(const S: string): Double;
{ The CORRECTLY ROUNDED double of a decimal string - the exact mirror of
  ExactRoundedDigits above, and written in the same idiom so that both can be
  ported to a target with no floating-point runtime of its own.

  ⭐ WHY THIS EXISTS, and it is two separate defects, both SILENT.
  VAL() used to end in FPC's Val(), and that route is wrong twice:

  1. THE 255-CHARACTER CLIFF. Val() on an AnsiString is fpc_Val_Real_AnsiStr,
     which reads "if length(S) > 255 then code := 256" and gives up - and our
     caller turns a non-zero code into 0.0. So VAL of a 256-character number
     answered ZERO. Measured: every length up to 255 worked, 256 and beyond
     returned 0, with nothing said. fbc gets these right.
  2. THE DOUBLE ROUNDING. FPC's val_real parses into ValReal, which on this
     target is the 80-bit Extended, and the result is then rounded again into
     the Double. Two roundings disagree with one on the values that land on a
     tie: measured 69 of 974 on a corpus built around exact midpoints, and
     about one in five thousand random long decimals. fbc is correctly rounded
     on all of them.
  ⛔ Note how this differs from the PRINT direction: there fbc double-rounds and
  WE are right, deliberately (see PIANO_FLOAT_PRINT.md). Here fbc and IEEE 754
  agree and we were the odd one out, so there is nothing to declare - this is
  simply a correction.

  THE METHOD, and it uses no floating point and no division of one big number by
  another. The value is a decimal integer P times a power of ten:
      V = P x 10^-j
  Multiplying P by 2^Sh (which the digit buffer can do) turns it into
      V = (P x 2^Sh) x 10^-j x 2^-Sh
  and dividing a DECIMAL number by 10^j is just reading the digits above index j
  - no division at all. So once the buffer has been grown until it holds at least
  21 digits above the point, the integer part I is exact and known to carry more
  than 64 bits, and everything below the point is folded into one sticky flag.
  I is then taken to binary the only way a decimal number goes to binary without
  a big divide: repeated division by 2^30, whose remainders ARE the 30-bit limbs.
  Rounding half-to-even on I plus the sticky bit is then ordinary integer work.

  The parse accepts what ParseLeadingFloat's scan produces and nothing more:
  [sign] digits [. digits] [ (e|E) [sign] digits ]. Anything else answers 0, the
  same as the non-zero Code it replaces. }
const
  MAXDIG   = 1500;    { worst case measured at ~1230; the slack is free }
  DIGCAP   = 800;     { significant digits KEPT. ⭐ Not an approximation: a
                        double's exact tie needs at most 767 significant
                        decimal digits, so beyond this a sticky flag carries
                        every bit of information the rest can hold. }
  P2CHUNK  = 30;
  P10CHUNK = 17;      { ⛔ not 18: the running carry can reach F, so the product
                        reaches 10F, and 10 x 10^18 overflows an Int64. }
  MAXLIMB  = 200;
var
  Buf: array[0..MAXDIG - 1] of Byte;      { little-endian decimal digits }
  Sig: array[0..DIGCAP - 1] of Byte;      { significant digits, most significant first }
  Limb: array[0..MAXLIMB - 1] of LongWord;
  Len, NLimb, SigCount, Exp10, DecExp, Prec, E, L, Drop: Integer;
  i, j, k, b, p, n, d, q, ev, esign, lo, hi, off: Integer;
  Neg, Sticky, Rest, SawDot, SawDigit: Boolean;
  Carry, T, Mul, Rem: Int64;
  Mant, Bits: QWord;
  c: Char;

  procedure MulBy(F: Int64);
  var
    x: Integer;
  begin
    Carry := 0;
    for x := 0 to Len - 1 do
    begin
      T := Int64(Buf[x]) * F + Carry;
      Buf[x] := Byte(T mod 10);
      Carry := T div 10;
    end;
    while (Carry > 0) and (Len < MAXDIG) do
    begin
      Buf[Len] := Byte(Carry mod 10);
      Carry := Carry div 10;
      Inc(Len);
    end;
  end;

  function GetBit(Pos: Integer): Integer;
  begin
    if (Pos < 0) or ((Pos div P2CHUNK) >= NLimb) then
      Result := 0
    else
      Result := Integer((Limb[Pos div P2CHUNK] shr (Pos mod P2CHUNK)) and 1);
  end;

begin
  Result := 0.0;
  n := Length(S);
  p := 1;
  Neg := False;
  if (p <= n) and ((S[p] = '+') or (S[p] = '-')) then
  begin
    Neg := (S[p] = '-');
    Inc(p);
  end;

  { The value is built as Sig (an integer of SigCount digits) x 10^Exp10. A digit
    before the point is part of that integer; one after it moves Exp10 down; a
    leading zero after the point moves Exp10 down without being stored. }
  SigCount := 0; Exp10 := 0; SawDot := False; SawDigit := False; Sticky := False;
  while p <= n do
  begin
    c := S[p];
    if (c >= '0') and (c <= '9') then
    begin
      SawDigit := True;
      d := Ord(c) - Ord('0');
      if (SigCount = 0) and (d = 0) then
      begin
        if SawDot then Dec(Exp10);
      end
      else if SigCount < DIGCAP then
      begin
        Sig[SigCount] := Byte(d);
        Inc(SigCount);
        if SawDot then Dec(Exp10);
      end
      else
      begin
        { past the cap: the digit cannot change the answer, only whether an exact
          tie is a tie }
        if d <> 0 then Sticky := True;
        if not SawDot then Inc(Exp10);
      end;
      Inc(p);
    end
    else if (c = '.') and (not SawDot) then
    begin
      SawDot := True;
      Inc(p);
    end
    else
      Break;
  end;
  if not SawDigit then Exit;

  if (p <= n) and ((S[p] = 'e') or (S[p] = 'E')) then
  begin
    q := p + 1;
    esign := 1;
    if (q <= n) and ((S[q] = '+') or (S[q] = '-')) then
    begin
      if S[q] = '-' then esign := -1;
      Inc(q);
    end;
    if (q <= n) and (S[q] >= '0') and (S[q] <= '9') then
    begin
      ev := 0;
      while (q <= n) and (S[q] >= '0') and (S[q] <= '9') do
      begin
        { clamped, not wrapped: a wild exponent still has to answer infinity or
          zero, and it must not overflow on the way there }
        if ev < 1000000 then ev := ev * 10 + (Ord(S[q]) - Ord('0'));
        Inc(q);
      end;
      Inc(Exp10, esign * ev);
      p := q;
    end;
  end;

  if SigCount = 0 then
  begin
    { ±0. The sign is kept: "-0" and "-0.0e5" answer negative zero, as fbc does. }
    if Neg then PInt64(@Result)^ := Int64($8000000000000000);
    Exit;
  end;

  { The decimal exponent of the leading digit. These two guards exist to BOUND
    THE BUFFER, not to decide anything: the ordinary path below still has to
    produce infinity for 1e309 and zero for 1e-324, and it does. }
  DecExp := (SigCount - 1) + Exp10;
  if DecExp > 330 then
  begin
    Bits := QWord($7FF0000000000000);
    if Neg then Bits := Bits or QWord($8000000000000000);
    Move(Bits, Result, SizeOf(Result));
    Exit;
  end;
  if DecExp < -400 then
  begin
    if Neg then PInt64(@Result)^ := Int64($8000000000000000);
    Exit;
  end;

  Len := SigCount;
  for i := 0 to SigCount - 1 do
    Buf[i] := Sig[SigCount - 1 - i];

  if Exp10 > 0 then
  begin
    i := Exp10;
    while i > 0 do
    begin
      k := i; if k > P10CHUNK then k := P10CHUNK;
      Mul := 1; for d := 1 to k do Mul := Mul * 10;
      MulBy(Mul);
      Dec(i, k);
    end;
    j := 0;
  end
  else
    j := -Exp10;

  { Grow until at least 21 digits sit ABOVE the point: 10^20 is past 2^66, so the
    integer part is guaranteed to carry more bits than the rounding needs. }
  k := 0;
  while ((Len - j) < 21) and (Len < MAXDIG - 12) do
  begin
    MulBy(Int64(1) shl P2CHUNK);
    Inc(k, P2CHUNK);
  end;

  { Everything below the point is one bit of information }
  if not Sticky then
    for i := 0 to j - 1 do
      if Buf[i] <> 0 then begin Sticky := True; Break; end;

  { The integer part to binary: divide by 2^30 and keep the remainders, which
    ARE the limbs. Dividing a decimal buffer from the top by F keeps every
    partial remainder below F, so each quotient digit stays below ten. }
  NLimb := 0;
  lo := j; hi := Len;
  while (hi > lo) and (NLimb < MAXLIMB) do
  begin
    Rem := 0;
    for i := hi - 1 downto lo do
    begin
      T := Rem * 10 + Int64(Buf[i]);
      Buf[i] := Byte(T shr P2CHUNK);
      Rem := T and ((Int64(1) shl P2CHUNK) - 1);
    end;
    Limb[NLimb] := LongWord(Rem);
    Inc(NLimb);
    while (hi > lo) and (Buf[hi - 1] = 0) do Dec(hi);
  end;

  { Unreachable by the bounds above (the grow loop guarantees digits above the
    point), but an empty limb array would index Limb[-1] rather than say so. }
  if NLimb = 0 then
  begin
    if Neg then PInt64(@Result)^ := Int64($8000000000000000);
    Exit;
  end;

  { The bit length of the integer part }
  L := (NLimb - 1) * P2CHUNK;
  b := P2CHUNK - 1;
  while (b >= 0) and (((Limb[NLimb - 1] shr b) and 1) = 0) do Dec(b);
  Inc(L, b + 1);

  E := (L - 1) - k;               { binary exponent of the leading bit }

  Prec := 53;
  if E < -1022 then Prec := 53 + (E + 1022);
  if Prec < 0 then
  begin
    { below half of the smallest subnormal: zero, with its sign }
    if Neg then PInt64(@Result)^ := Int64($8000000000000000);
    Exit;
  end;

  Drop := L - Prec;
  Mant := 0;
  for b := Prec - 1 downto 0 do
    Mant := (Mant shl 1) or QWord(GetBit(Drop + b));

  { Round half to EVEN, and the tie test looks at every bit below - the whole
    reason this function exists. }
  Rest := Sticky;
  if (not Rest) and (Drop >= 2) then
  begin
    q := (Drop - 1) div P2CHUNK;
    for i := 0 to q - 1 do
      if Limb[i] <> 0 then begin Rest := True; Break; end;
    if not Rest then
    begin
      off := (Drop - 1) mod P2CHUNK;
      if (off > 0) and (q < NLimb) then
        if (Limb[q] and ((LongWord(1) shl off) - 1)) <> 0 then Rest := True;
    end;
  end;
  if (Drop > 0) and (GetBit(Drop - 1) = 1) and (Rest or ((Mant and 1) <> 0)) then
    Inc(Mant);

  if E >= -1022 then
  begin
    { the carry can push the mantissa into the next binade }
    if Mant = (QWord(1) shl 53) then
    begin
      Mant := QWord(1) shl 52;
      Inc(E);
    end;
    if E > 1023 then
      Bits := QWord($7FF0000000000000)
    else
      Bits := (QWord(E + 1023) shl 52) or (Mant and QWord($000FFFFFFFFFFFFF));
  end
  else
    { Subnormal: the value is Mant x 2^-1074 by construction, which IS the bit
      pattern. ⭐ And when the rounding carried it up to 2^52 the same formula
      gives the smallest NORMAL number, which is exactly right. }
    Bits := Mant;

  if Neg then Bits := Bits or QWord($8000000000000000);
  Move(Bits, Result, SizeOf(Result));
end;

procedure TConsoleBehavior.SetFloatDigits(V: Integer);
begin
  if V < 1 then V := 1;
  if V > MAX_FLOAT_DIGITS then V := MAX_FLOAT_DIGITS;
  FFloatDigits := V;
end;

procedure TConsoleBehavior.SetSingleDigits(V: Integer);
begin
  if V < 1 then V := 1;
  if V > MAX_FLOAT_DIGITS then V := MAX_FLOAT_DIGITS;
  FSingleDigits := V;
end;

function FormatDoubleFB(Value: Double; SIGDIGITS: Integer = 16): string;
// FreeBASIC prints a DOUBLE with 16 significant digits -- "3.141592653589793", and it shows the 16th
// digit even when it is representation noise ("0.9999999999999999", "44.99999999999999"). A SINGLE gets
// 7 (its 24-bit mantissa is worth about 7.2 decimal digits), which is what hides its representation
// error: an accumulator holding 8.300000190734863 prints as "8.3", exactly as FreeBASIC shows it.
//
// The DIGITS now come from ExactRoundedDigits: the exact binary value, rounded ONCE, half to even, as
// IEEE 754-2019 sec.5.12.2 asks for. They used to come from FPC's Str (17 digits) and be rounded again
// to 16 here, which is a DOUBLE ROUNDING and disagrees with the correct answer on 4.75% of doubles.
// ⛔ fbc double-rounds too, so this DELIBERATELY DIFFERS FROM fbc on those values. It is not a
// regression and it is not to be "fixed" back -- read job/docs/PIANO_FLOAT_PRINT.md first.
//
// The fixed/exponential choice is still %g's, from the decimal exponent alone: exponential when the
// exponent is < -4 or >= SIGDIGITS, with a signed exponent of AT LEAST THREE digits ("1e+016",
// "1e-005", "1e+300"). The exponent comes from the ROUNDED digits, because a carry can move the value
// into the next decade and across that boundary.
var
  Digits, S: string;
  Ex, i: Integer;
  Neg: Boolean;
begin
  if Value = 0 then Exit('0');          // -0 gets its sign back in FormatNumber
  Neg := PInt64(@Value)^ < 0;
  Digits := ExactRoundedDigits(Value, SIGDIGITS, Ex);

  if (Ex >= -4) and (Ex < SIGDIGITS) then
  begin
    // FIXED. Place the point after digit Ex+1; a negative Ex means leading zeros.
    if Ex >= 0 then
    begin
      S := Copy(Digits, 1, Ex + 1);
      if Length(S) < Ex + 1 then S := S + StringOfChar('0', Ex + 1 - Length(S));
      if Length(Digits) > Ex + 1 then
        S := S + '.' + Copy(Digits, Ex + 2, Length(Digits) - Ex - 1);
    end
    else
      S := '0.' + StringOfChar('0', -Ex - 1) + Digits;
    if Pos('.', S) > 0 then
    begin
      while (S <> '') and (S[Length(S)] = '0') do SetLength(S, Length(S) - 1);
      if (S <> '') and (S[Length(S)] = '.') then SetLength(S, Length(S) - 1);
    end;
    if S = '' then S := '0';
    if Neg then S := '-' + S;
    Exit(S);
  end;

  // EXPONENTIAL, FreeBASIC style: "1e+016", "1.234568e+010", "5e-005", "1e+300".
  i := Length(Digits);
  while (i > 1) and (Digits[i] = '0') do Dec(i);
  S := Copy(Digits, 1, 1);
  if i > 1 then S := S + '.' + Copy(Digits, 2, i - 1);
  Result := IntToStr(Abs(Ex));
  while Length(Result) < 3 do Result := '0' + Result;      // at least three digits
  if Ex >= 0 then Result := S + 'e+' + Result else Result := S + 'e-' + Result;
  if Neg then Result := '-' + Result;
end;

{ TConsoleBehavior }

constructor TConsoleBehavior.Create;
begin
  inherited Create;

  // Default: comportamento Commodore 64
  FScreenCols := 40;
  FScreenRows := 25;

  // The dialect's display precision. A preset may change the number FORMAT but
  // never these: how many digits to show is a property of the language, not of
  // the machine being emulated.
  FFloatDigits := 16;
  FSingleDigits := 7;

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

function TConsoleBehavior.FormatNumber(Value: Double; AsSingle: Boolean = False): string;
var
  Prefix, Suffix, NumStr: string;
  Bits: Int64;
  NonNeg, IsNanV, IsInfV: Boolean;
begin
  Prefix := '';
  Suffix := '';

  // Classify NaN/Infinity by IEEE-754 bit pattern (a MODERN float division by zero yields these). FP
  // comparisons and Frac/FloatToStr on a NaN raise EInvalidOp because FPC leaves FP exceptions unmasked,
  // so the sign and special-value tests must inspect the raw bits, never do arithmetic on Value.
  Bits := PInt64(@Value)^;
  IsNanV := ((Bits shr 52) and $7FF = $7FF) and ((Bits and $000FFFFFFFFFFFFF) <> 0);
  IsInfV := ((Bits shr 52) and $7FF = $7FF) and ((Bits and $000FFFFFFFFFFFFF) =  0);
  if IsNanV then
    NonNeg := Bits >= 0            // sign bit (bit 63); a NaN cannot be ordered-compared safely
  else if FNumberFormat = nfFreeBASIC then
    NonNeg := Bits >= 0           // FreeBASIC: negative zero prints "-0" (sign bit, not the ordered compare)
  else
    NonNeg := Value >= 0;         // CLASSIC: ordered comparison (v7 has no "-0"); safe for finite and +/-Inf

  case FNumberFormat of
    nfCommodore, nfMSX:
      begin
        // Spazio prima se positivo (al posto del segno -)
        if NonNeg then
          Prefix := ' '
        else
          Prefix := '';  // Il segno - è già nella rappresentazione

        // Spazio dopo sempre
        Suffix := ' ';
      end;
    nfQB:
      begin
        // -lang qb, FLOAT: the sign pad, and NOTHING after it. Only integers get the trailing space.
        if NonNeg then Prefix := ' ' else Prefix := '';
        Suffix := '';
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
        if FNumberSpaceBefore and NonNeg then
          Prefix := ' ';
        if FNumberSpaceAfter then
          Suffix := ' ';
      end;

    nfFreeBASIC:
      begin
        if NonNeg then Prefix := ' ';   // left padding for the sign
        Suffix := '';                   // no trailing space (that is the -lang qb behaviour)
      end;
  end;

  // NaN/Infinity must skip Frac/FloatToStr (both trap on non-finite input). FreeBASIC does not render
  // these itself: it hands them to the platform's C library, so its own output DIFFERS by platform --
  // MSVCRT on Windows prints "1.#INF" / "-1.#IND" / "1.#QNAN", glibc on Linux prints "inf" / "-nan" /
  // "nan". Mirroring the platform is therefore not a portability compromise: it is what makes us agree
  // with the fbc of whichever machine we are on. (Verified against fbc 1.10.1 on win64.) The regression
  // harness folds the two spellings together, so the corpus stays platform-independent.
  //
  // The sign bit tells the two NaNs apart: SET is the "indefinite" an invalid operation produces (0/0,
  // Sqr of a negative); CLEAR is the quiet NaN the C library's log(-1) returns.
  if IsNanV then
  begin
    {$IFDEF WINDOWS}
    if NonNeg then NumStr := '1.#QNAN' else NumStr := '-1.#IND';
    {$ELSE}
    if NonNeg then NumStr := 'nan' else NumStr := '-nan';
    {$ENDIF}
  end
  else if IsInfV then
  begin
    {$IFDEF WINDOWS}
    if NonNeg then NumStr := '1.#INF' else NumStr := '-1.#INF';
    {$ELSE}
    if NonNeg then NumStr := 'inf' else NumStr := '-inf';
    {$ENDIF}
  end
  // A DOUBLE prints with 16 significant digits in FreeBASIC; FPC's FloatToStr gives 15, which rounded
  // the last digit away and made every high-precision program disagree with the reference in its final
  // place. CLASSIC keeps FloatToStr: the Commodore ROM's own precision is lower still, and widening it
  // there would change v7 output for no reason.
  //
  // This comes BEFORE the integral-value shortcut below, which used to pre-empt it: FreeBASIC chooses
  // fixed vs exponential from the MAGNITUDE alone, and 1e16 is integral and fits an Int64 yet still prints
  // as "1e+016". FormatDoubleFB renders a whole number just as well ("1000000000000000"), so nothing needs
  // the shortcut here -- and going through it was what made every double past 2^63 print the Int64 overflow
  // -9223372036854775808.
  else if FNumberFormat = nfFreeBASIC then
  begin
    // The digit COUNT comes from the behavior ("OPTION DIGITS n"); the defaults
    // are the dialect's - 16 for a Double, 7 for a Single, whose 24-bit mantissa
    // is worth about 7.2 decimal digits. Whatever the count, the digits are
    // correctly rounded from the exact value.
    if AsSingle then
      NumStr := FormatDoubleFB(Value, FSingleDigits)
    else
      NumStr := FormatDoubleFB(Value, FFloatDigits);
  end
  // Round() traps (or, under {$Q-}, silently yields Int64.Min) once the value is past 2^63, so the whole
  // number has to fit an Int64 to be printed as one. Beyond that, FloatToStr's exponential form.
  else if (Frac(Value) = 0) and (Abs(Value) < 9223372036854775808.0) then
    NumStr := IntToStr(Round(Value))
  else
    NumStr := FloatToStr(Value);

  // Negative zero: its magnitude renders as "0" with no sign, but NonNeg is False (from the sign bit under
  // nfFreeBASIC), so restore the minus FreeBASIC prints ("-0"). Any ordinary negative already carries it.
  if (not NonNeg) and (NumStr <> '') and (NumStr[1] <> '-') then NumStr := '-' + NumStr;
  Result := Prefix + NumStr + Suffix;
end;

function TConsoleBehavior.FormatUInt(Value: QWord): string;
// Like FormatNumber but for an exact unsigned 64-bit value (it is always >= 0, so it gets the
// same leading space a positive number would). Formatting via QWord avoids the Double precision
// loss FormatNumber would suffer for values above 2^53.
var
  Prefix, Suffix: string;
begin
  Prefix := '';
  Suffix := '';
  case FNumberFormat of
    nfCommodore, nfMSX,
    // The qb dialect has no unsigned types (fbc rejects "As UInteger" outright), so this arm is
    // unreachable; it sits with the integers so the preset is never silently half-defined.
    nfQB:
      begin Prefix := ' '; Suffix := ' '; end;     // non-negative -> leading space
    nfSpectrum:
      begin Prefix := ''; Suffix := ''; end;
    nfAtari:
      begin Prefix := ''; Suffix := ' '; end;
    nfCustom:
      begin
        if FNumberSpaceBefore then Prefix := ' ';
        if FNumberSpaceAfter then Suffix := ' ';
      end;
    nfFreeBASIC:
      // The FB manual's Print page, under "Differences from QB": "Unsigned numbers are printed without
      // a space before them." So an unsigned gets neither the sign padding nor a trailing space.
      begin Prefix := ''; Suffix := ''; end;
  end;
  Result := Prefix + UIntToStr(Value) + Suffix;
end;

function TConsoleBehavior.FormatInt(Value: Int64): string;
// Exact signed 64-bit formatting with the same PRINT spacing as FormatNumber, but without routing the
// value through a Double (which rounds integers above 2^53). Non-negative gets the leading space that
// stands in for the sign; a negative value already carries its '-'.
var
  Prefix, Suffix: string;
  NonNeg: Boolean;
begin
  Prefix := '';
  Suffix := '';
  NonNeg := Value >= 0;
  case FNumberFormat of
    nfCommodore, nfMSX, nfQB:
      begin
        if NonNeg then Prefix := ' ';   // leading space in place of the sign
        Suffix := ' ';
      end;
    nfSpectrum:
      begin Prefix := ''; Suffix := ''; end;
    nfAtari:
      begin Prefix := ''; Suffix := ' '; end;
    nfCustom:
      begin
        if FNumberSpaceBefore and NonNeg then Prefix := ' ';
        if FNumberSpaceAfter then Suffix := ' ';
      end;
    nfFreeBASIC:
      begin
        if NonNeg then Prefix := ' ';   // left padding for the sign
        Suffix := '';                   // no trailing space (that is the -lang qb behaviour)
      end;
  end;
  Result := Prefix + IntToStr(Value) + Suffix;
end;

function TConsoleBehavior.FormatString(const S: string): string;
var
  Prefix, Suffix: string;
begin
  // The spaces around a printed string are a COMMODORE trait; in the FreeBASIC dialect both flags are
  // off and this is the identity. It was still written as "'' + S + ''", which is a concatenation:
  // one allocation and a full copy of S on EVERY Print, paid by every MODERN program for a rule that
  // does not apply to it.
  if (not FStringSpaceBefore) and (not FStringSpaceAfter) then
    Exit(S);

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
