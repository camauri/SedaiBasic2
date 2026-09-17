{
  SedaiFbFormat - string.bi's Format(), as fbc's runtime answers it

  Copyright (C) 2026 Maurizio Cammalleri
  Released under GNU GPL v3 (see LICENCE.md)

  ⛔ WHY THIS UNIT EXISTS (17 Sep 2026, the `string` library deck). Our FORMAT was written from the manual and
  answered differently from fbc on most of a 21-mask sweep: 1e21 overflowed to -9223372036854775808, 12.345
  rounded to 12.35 where fbc keeps 12.34 (it drops the fraction of the fraction, as VBDOS did), a quoted
  literal, a "\" escape, the ";" and the "ddddd"/"ttttt" masks were read another way. The rules are fbc
  1.10.1's str_format.c, re-written in Pascal step for step - its quirks included, because the product is
  measured against fbc and a user moving a program between the two must get the same text.
  ⭐ The C runtime does the three things whose ROUNDING decides the digits: "%.*f", pow() and log10(). fbc
  uses exactly those, so calling them is the only way to get the same last digit; re-deriving them in
  Pascal would be an approximation of an approximation.
  ⚠️ The separators are the C locale's (fbc programs never call setlocale): "." "," "/" ":", short date
  "MM/dd/yy", short time "HH:mm:ss", English names.
}
unit SedaiFbFormat;

{$mode objfpc}{$H+}

interface

function FbFormat(Value: Double; const Mask: string): string;

implementation

uses
  SysUtils, SedaiFbDate;

{$IFDEF WINDOWS}
const CRT = 'msvcrt.dll';
function c_snprintf(Buf: PChar; N: SizeUInt; Fmt: PChar): LongInt; cdecl; varargs; external CRT name '_snprintf';
{$ELSE}
const CRT = 'c';
function c_snprintf(Buf: PChar; N: SizeUInt; Fmt: PChar): LongInt; cdecl; varargs; external CRT name 'snprintf';
{$LINKLIB m}
{$ENDIF}
function c_pow(X, Y: Double): Double; cdecl; external {$IFDEF WINDOWS}CRT{$ELSE}'m'{$ENDIF} name 'pow';
function c_log10(X: Double): Double; cdecl; external {$IFDEF WINDOWS}CRT{$ELSE}'m'{$ENDIF} name 'log10';

const
  FB_MAXFIXLEN = 19;
  MONTHS_LONG: array[1..12] of string = ('January', 'February', 'March', 'April', 'May', 'June', 'July',
    'August', 'September', 'October', 'November', 'December');
  DAYS_LONG: array[1..7] of string = ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');

type
  TMaskType = (mtUnknown, mtNumber, mtDateTime);
  TMaskInfo = record
    MaskType: TMaskType;
    HasDecimalPoint, HasThousandSep, HasPercent, HasExponent, ExponentAddPlus: Boolean;
    HasSign, SignAddPlus, HasAmPm: Boolean;
    NumDigitsFix, NumDigitsFrac, NumDigitsOmit, ExpDigits: Integer;
    LengthMin, LengthOpt: Int64;
  end;

function FixedText(X: Double; Precision: Integer): string;
var
  Buf: array[0..511] of Char;
  N: LongInt;
begin
  N := c_snprintf(@Buf[0], SizeOf(Buf), '%.*f', LongInt(Precision), X);
  if N < 0 then N := 0;
  if N >= SizeOf(Buf) then N := SizeOf(Buf) - 1;
  SetString(Result, PChar(@Buf[0]), N);
end;

function IntLog10_64(X: QWord): Integer;
const
  P: array[0..19] of QWord = (1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
    10000000000, 100000000000, 1000000000000, 10000000000000, 100000000000000, 1000000000000000,
    10000000000000000, 100000000000000000, 1000000000000000000, QWord(10000000000000000000));
var
  I: Integer;
begin
  for I := 19 downto 0 do
    if X >= P[I] then Exit(I);
  Result := -1;
end;

// (unsigned long long) of a non-negative double, as C converts it (truncation; no Int64 overflow below 2^64).
function DoubleToQWord(X: Double): QWord;
begin
  if X <= 0 then Exit(0);
  // ⚠️ At 2^64 and above the C conversion is undefined, and what gcc's x86-64 code does is subtract 2^63, meet
  // the "integer indefinite" value and XOR it away: ZERO. fbc prints Format(1e21) as an empty string because of
  // it, and that is the answer measured here.
  if X >= 18446744073709551616.0 then Exit(0);
  if X >= 9223372036854775808.0 then
    Result := QWord(Trunc(X - 9223372036854775808.0)) + QWord($8000000000000000)
  else
    Result := QWord(Trunc(X));
end;

// modf: the fraction keeps the sign of the number, the whole part is returned through Whole.
function ModF(X: Double; out Whole: Double): Double;
begin
  Whole := Int(X);
  Result := X - Whole;
end;

procedure GetNumberParts(Number: Double; out FixPart: string; out FracPart: string; out Sign: Char;
  Precision: Integer);
var
  DblFix, DblFrac: Double;
  Neg: Boolean;
  UFix: QWord;
  S: string;
  StartPos, EndPos: Integer;   // 1-based, EndPos one past the last kept character
begin
  DblFrac := ModF(Number, DblFix);
  Neg := Number < 0.0;
  if Neg then UFix := DoubleToQWord(-DblFix) else UFix := DoubleToQWord(DblFix);
  if DblFrac < 0.0 then DblFrac := -DblFrac;
  S := FixedText(DblFrac, Precision);
  // Trailing zeroes go, and the decimal point with them when nothing else is left. The integer digit is
  // skipped WITHOUT being read: a fraction that rounds to "1.00" loses its carry, as fbc's does.
  StartPos := 1;
  if (Length(S) >= 1) and (S[1] = '-') then Inc(StartPos);
  Inc(StartPos);
  EndPos := Length(S) + 1;
  while EndPos <> StartPos do
  begin
    Dec(EndPos);
    if S[EndPos] <> '0' then
    begin
      if S[EndPos] <> '.' then
      begin
        Inc(StartPos);
        Inc(EndPos);
      end;
      Break;
    end;
  end;
  if EndPos > StartPos then FracPart := Copy(S, StartPos, EndPos - StartPos) else FracPart := '';
  if (UFix = 0) and Neg then
  begin
    FixPart := ''; Sign := '-';
  end
  else if (UFix = 0) and (Number > 0.0) then
  begin
    FixPart := ''; Sign := '+';
  end
  else
  begin
    if Neg then Sign := '-'
    else if UFix > 0 then Sign := '+'
    else Sign := #0;
    FixPart := IntToStr(UFix);
  end;
end;

function BuildDouble(Num: Double): string;
var
  FixPart, FracPart: string;
  Sign: Char;
begin
  GetNumberParts(Num, FixPart, FracPart, Sign, 11);
  Result := '';
  if Sign = '-' then Result := '-';
  Result := Result + FixPart;
  if FracPart <> '' then Result := Result + '.' + FracPart;
end;

function HRound(Value: Double; const Info: TMaskInfo): Double;
var
  Fix, Fr, P10, FracFrac, Whole: Double;
  IntFrac: Int64;
begin
  Fr := ModF(Value, Fix);
  if Info.NumDigitsFrac = 0 then
  begin
    IntFrac := Trunc(Fr * 1.0E+15);
    if IntFrac > Trunc(5.0E+14) then Value := Int(Value) + Ord(Frac(Value) > 0)   // ceil
    else if IntFrac < -Trunc(5.0E+14) then Value := Int(Value) - Ord(Frac(Value) < 0);   // floor
  end
  else if Fr <> 0.0 then
  begin
    // The fraction of the fraction is REMOVED, not rounded into it (VBDOS: 2.55 -> 2.5).
    P10 := c_pow(10.0, Info.NumDigitsFrac);
    FracFrac := ModF(Fr * P10, Whole);
    Fr := Whole;
    IntFrac := Trunc(FracFrac * (1.0E+15 / P10));
    if IntFrac > Trunc(5.0E+14 / P10) then Fr := Fr + 1.0
    else if IntFrac < -Trunc(5.0E+14 / P10) then Fr := Fr - 1.0;
    Fr := Fr / P10;
    Value := Fix + Fr;
  end;
  Result := Value;
end;

function HourOf(V: Double): Integer;
var H, N, S: Integer;
begin
  FbDecodeTimeQB(V, H, N, S); Result := H;
end;

function MinuteOf(V: Double): Integer;
var H, N, S: Integer;
begin
  FbDecodeTimeQB(V, H, N, S); Result := N;
end;

function SecondOf(V: Double): Integer;
var H, N, S: Integer;
begin
  FbDecodeTimeQB(V, H, N, S); Result := S;
end;

function Pad2(N: Integer): string;
begin
  Result := Format('%.2d', [N]);
end;

function ProcessMask(DoOutput: Boolean; const Mask: string; Value: Double; var Info: TMaskInfo;
  out Output: string): Boolean; forward;

function FormatWith(Value: Double; const Mask: string): string;
var
  Info: TMaskInfo;
begin
  Result := '';
  if Mask = '' then Exit(BuildDouble(Value));
  if ProcessMask(False, Mask, Value, Info, Result) then
    ProcessMask(True, Mask, Value, Info, Result)
  else
    Result := '';
end;

function ProcessMask(DoOutput: Boolean; const Mask: string; Value: Double; var Info: TMaskInfo;
  out Output: string): Boolean;
const
  THOUSANDS_SEP = ',';
  DECIMAL_POINT = '.';
  DATE_SEP = '/';
  TIME_SEP = ':';
var
  FixPart, FracPart, ExpPart: string;
  Sign: Char;
  LenFix, LenFrac, LenExp, IndexFix, IndexFrac, IndexExp: Integer;
  ExpValue, ExpAdjust, NumSkipFix, NumSkipExp, NonZero: Int64;
  DoSkip, DoExp, DoString, DidSign, DidExp, DidHour, DidThousandSep: Boolean;
  DoNumFrac, LastWasComma, WasKDiv, DoAdd, OldDidHour, AmPmSmall: Boolean;
  MaskLen, I, K, Count, Remaining, Hour, Wd, M: Integer;
  Cur: Char;
  Add: string;

  function At(Idx: Integer): Char;   // 0-based, NUL past the end as in C
  begin
    if (Idx >= 0) and (Idx < MaskLen) then Result := Mask[Idx + 1] else Result := #0;
  end;

  procedure SetExpPart;
  begin
    ExpPart := IntToStr(Integer(ExpValue));
    LenExp := Length(ExpPart);
    if ExpValue < 0 then begin IndexExp := 1; ExpAdjust := 1; end
    else begin IndexExp := 0; ExpAdjust := 0; end;
    NumSkipExp := Info.ExpDigits - (LenExp - ExpAdjust);
  end;

begin
  Result := True;
  Output := '';
  MaskLen := Length(Mask);
  FixPart := ''; FracPart := ''; ExpPart := '';
  Sign := #0;
  LenFix := 0; LenFrac := 0; LenExp := 0; IndexExp := 0;
  ExpAdjust := 0; NumSkipFix := 0; NumSkipExp := 0;
  DoSkip := False; DoExp := False; DoString := False;
  DidSign := False; DidExp := False; DidHour := False; DidThousandSep := False;
  DoNumFrac := False; LastWasComma := False; WasKDiv := False; DoAdd := False;

  if not DoOutput then
    FillChar(Info, SizeOf(Info), 0)
  else if Info.MaskType = mtNumber then
  begin
    if Info.HasPercent then Value := Value * 100.0;
    Value := Value / c_pow(10.0, Info.NumDigitsOmit);
  end;

  if Value <> 0.0 then
  begin
    ExpValue := Trunc(Int(c_log10(Abs(Value))) - Ord(Frac(c_log10(Abs(Value))) < 0)) + 1;   // floor + 1
    NonZero := 1;
  end
  else
  begin
    ExpValue := 0;
    NonZero := 0;
  end;

  if DoOutput then
  begin
    if Info.MaskType = mtNumber then
    begin
      if Info.HasExponent then
      begin
        if NonZero <> 0 then Dec(ExpValue, Info.NumDigitsFix);
        if ExpValue <> 0 then
        begin
          if -ExpValue <= 308 then
            Value := Value * c_pow(10.0, -ExpValue)
          else
          begin
            Value := Value * c_pow(5.0, -ExpValue);
            Value := Value * c_pow(2.0, -ExpValue);
          end;
        end;
        while Value >= 18446744073709551616.0 do
        begin
          Value := Value / 10.0;
          Inc(ExpValue);
        end;
        SetExpPart;
      end
      else if ExpValue < 0 then
      begin
        if -ExpValue >= Info.NumDigitsFrac then
        begin
          Value := 0.0;
          ExpValue := 0;
        end;
      end
      else
      begin
        if ExpValue > FB_MAXFIXLEN then
        begin
          Dec(ExpValue, FB_MAXFIXLEN);
          Value := Value * c_pow(10.0, -ExpValue);
        end
        else
          ExpValue := 0;
      end;

      Value := HRound(Value, Info);

      if Info.HasExponent and (IntLog10_64(DoubleToQWord(Abs(Value))) = Info.NumDigitsFix) then
      begin
        Value := Value / 10.0;
        Inc(ExpValue);
        SetExpPart;
      end;

      GetNumberParts(Value, FixPart, FracPart, Sign, Info.NumDigitsFrac);
      LenFix := Length(FixPart);
      LenFrac := Length(FracPart);

      if (ExpValue > 0) and not Info.HasExponent then
      begin
        FixPart := FixPart + StringOfChar('0', ExpValue);
        LenFix := Length(FixPart);
      end;

      NumSkipFix := Info.NumDigitsFix - LenFix;
    end;
  end
  else
  begin
    if ExpValue > FB_MAXFIXLEN then LenFix := ExpValue else LenFix := FB_MAXFIXLEN;
    LenFrac := 0;
  end;

  IndexFix := 0; IndexFrac := 0;
  I := 0;
  while I < MaskLen do
  begin
    Add := At(I);
    Cur := At(I);
    if DoSkip then
    begin
      DoSkip := False;
      if not DoOutput then Inc(Info.LengthMin) else DoAdd := True;
    end
    else if DoExp then
    begin
      if not DoOutput then
      begin
        Info.HasExponent := True;
        case Cur of
          '-': begin Info.ExponentAddPlus := False; Inc(Info.LengthOpt); end;
          '+': begin Info.ExponentAddPlus := True; Inc(Info.LengthMin); end;
        else
          Exit(False);
        end;
      end
      else if Info.ExponentAddPlus or (ExpValue < 0) then
      begin
        if ExpValue < 0 then Add := '-' else Add := '+';
        DoAdd := True;
      end;
      DoExp := False;
      DidExp := True;
      DoNumFrac := False;
    end
    else if DoString then
    begin
      if Cur = '"' then DoString := False
      else if not DoOutput then Inc(Info.LengthMin)
      else DoAdd := True;
    end
    else
    begin
      if DoOutput and (Cur in ['.', '#', '0']) then
      begin
        if (not Info.HasSign) and (not DidSign) then
        begin
          DidSign := True;
          if Info.SignAddPlus or (Sign = '-') then
          begin
            Add := Sign;
            DoAdd := True;
          end
          else
            Continue;   // the same mask character again, now with the sign done (C: --i; continue)
        end
        else if NumSkipFix < 0 then
        begin
          // FixPart holds more digits than the mask has places: they come out here, in groups.
          if Info.HasThousandSep then
          begin
            Remaining := LenFix - IndexFix;
            if (IndexFix <> LenFix) and (Remaining mod 3 = 0) then
            begin
              if DidThousandSep then
              begin
                DidThousandSep := False;
                Add := Copy(FixPart, IndexFix + 1, 3);
              end
              else if IndexFix >= 1 then
              begin
                DidThousandSep := True;
                Add := THOUSANDS_SEP;
              end
              else
                Add := Copy(FixPart, IndexFix + 1, 1);   // pszAdd, LenAdd 1 (C leaves it at its default)
            end
            else
              Add := Copy(FixPart, IndexFix + 1, Remaining mod 3);
          end
          else
            Add := Copy(FixPart, IndexFix + 1, -NumSkipFix);
          DoAdd := True;
          if not DidThousandSep then
          begin
            Inc(IndexFix, Length(Add));
            Inc(NumSkipFix, Length(Add));
          end;
        end;
      end;
      // After an addition here the mask character is read again on the next turn (C: --i before the loop's ++i),
      // and the classification below is skipped for this turn.
      if DoAdd then Dec(I)
      else
      begin
      case Cur of
        '%', ',', '#', '0', '+', 'E', 'e', '-', '.':
          if Info.MaskType = mtUnknown then Info.MaskType := mtNumber;
        'd', 'n', 'm', 'M', 'y', 'h', 'H', 's', 't', ':', '/':
          if Info.MaskType = mtUnknown then Info.MaskType := mtDateTime;
      end;

      case Cur of
        '%':
          if not DoOutput then
          begin
            if Info.MaskType = mtNumber then
            begin
              if not Info.HasPercent then
              begin
                Info.HasPercent := True;
                Inc(Info.LengthMin);
              end
              else
                Exit(False);
            end
            else
              Inc(Info.LengthMin);
          end
          else
            DoAdd := True;
        '.':
          begin
            if not DoOutput then
            begin
              if (Info.MaskType = mtNumber) and not Info.HasDecimalPoint then
              begin
                Info.HasDecimalPoint := True;
                if LastWasComma then
                begin
                  Inc(Info.NumDigitsOmit, 3);
                  WasKDiv := True;
                end
                else if Info.NumDigitsOmit <> 0 then
                  Inc(Info.NumDigitsOmit, 3);
              end;
              Inc(Info.LengthMin);
            end
            else
            begin
              DoAdd := True;
              if Info.MaskType = mtNumber then Add := DECIMAL_POINT;
            end;
            DoNumFrac := True;
          end;
        ',':
          if not DoOutput then
          begin
            if Info.MaskType = mtNumber then
            begin
              if LastWasComma then
              begin
                Inc(Info.NumDigitsOmit, 3);
                WasKDiv := True;
              end;
              LastWasComma := True;
            end
            else
              Inc(Info.LengthMin);
          end
          else if Info.MaskType = mtNumber then
          begin
            if LastWasComma then WasKDiv := True;
            LastWasComma := True;
          end
          else
            DoAdd := True;
        '#', '0':
          if not DoOutput then
          begin
            if Info.MaskType = mtNumber then
            begin
              if DoNumFrac then Inc(Info.NumDigitsFrac)
              else if DidExp then Inc(Info.ExpDigits)
              else Inc(Info.NumDigitsFix);
              if Cur = '#' then Inc(Info.LengthOpt) else Inc(Info.LengthMin);
            end
            else
              Inc(Info.LengthMin);
          end
          else if Info.MaskType = mtNumber then
          begin
            if DoNumFrac then
            begin
              if IndexFrac <> LenFrac then
              begin
                Add := FracPart[IndexFrac + 1];
                Inc(IndexFrac);
                DoAdd := True;
              end
              else if Cur = '0' then
                DoAdd := True;
            end
            else if DidExp then
            begin
              if NumSkipExp > 0 then
              begin
                if Cur = '0' then DoAdd := True;
                Dec(NumSkipExp);
              end
              else if IndexExp <> LenExp then
              begin
                Add := ExpPart[IndexExp + 1];
                Inc(IndexExp);
                if ((IndexExp - ExpAdjust) >= Info.ExpDigits) and (IndexExp <> LenExp) then
                  Dec(I);   // more exponent digits than places: they all come out on this character
                DoAdd := True;
              end;
            end
            else
            begin
              if Info.HasThousandSep then
              begin
                Remaining := LenFix - IndexFix + NumSkipFix;
                if Remaining mod 3 = 0 then
                begin
                  if DidThousandSep then
                    DidThousandSep := False
                  else if (NumSkipFix = 0) and (IndexFix <> 0) then
                  begin
                    DidThousandSep := True;
                    Add := THOUSANDS_SEP;
                    DoAdd := True;
                    Dec(I);
                  end;
                end;
              end;
              if not DoAdd then
              begin
                if NumSkipFix <> 0 then
                begin
                  if Cur = '0' then DoAdd := True;
                  Dec(NumSkipFix);
                end
                else if IndexFix <> LenFix then
                begin
                  Add := FixPart[IndexFix + 1];
                  Inc(IndexFix);
                  DoAdd := True;
                end
                else if Cur = '0' then
                  DoAdd := True;
              end;
            end;
          end
          else
            DoAdd := True;
        'E', 'e':
          if Info.MaskType = mtNumber then
          begin
            if not DidExp then
            begin
              DoExp := True;
              if not DoOutput then Inc(Info.LengthMin) else DoAdd := True;
            end
            else
              Exit(False);
          end
          else if not DoOutput then Inc(Info.LengthMin)
          else DoAdd := True;
        '\':
          DoSkip := True;
        '*', '$', '(', ')', ' ', #9:
          if not DoOutput then Inc(Info.LengthMin) else DoAdd := True;
        '+', '-':
          if not DoOutput then
          begin
            Inc(Info.LengthMin);
            if not Info.HasSign then
            begin
              Info.HasSign := True;
              Info.SignAddPlus := Cur = '+';
            end;
          end
          else if Info.MaskType = mtDateTime then
            DoAdd := True
          else if not DidSign then
          begin
            DidSign := True;
            if Info.SignAddPlus or (Sign = '-') then
            begin
              Add := Sign;
              DoAdd := True;
            end;
          end
          else
            DoAdd := True;
        'd', 'm', 'n', 'M', 'y', 'h', 'H', 's', 't':
          if Info.MaskType = mtDateTime then
          begin
            OldDidHour := DidHour;
            Count := 1;
            while At(I + Count) = Cur do Inc(Count);
            DidHour := False;
            if (Cur = 'm') and ((Count > 2) or not OldDidHour) then Cur := 'M';
            if (Cur = 't') and (Count = 5) then
            begin
              Inc(I, Count - 1);
              Add := FormatWith(Value, 'HH:mm:ss');
              if not DoOutput then Inc(Info.LengthMin, Length(Add)) else DoAdd := True;
            end
            else if (Cur = 't') and ((Count = 1) or (Count = 2)) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then
              begin
                Inc(Info.LengthMin, Count);
                Info.HasAmPm := True;
              end
              else
              begin
                if HourOf(Value) >= 12 then Add := Copy('PM', 1, Count) else Add := Copy('AM', 1, Count);
                DoAdd := True;
              end;
            end
            else if (Cur = 'd') and (Count = 5) then
            begin
              Inc(I, Count - 1);
              Add := FormatWith(Value, 'MM/dd/yy');
              if not DoOutput then Inc(Info.LengthMin, Length(Add)) else DoAdd := True;
            end
            else if (Cur = 'd') and (Count <= 2) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then
              begin
                Inc(Info.LengthMin, Count);
                if Count = 1 then Inc(Info.LengthOpt);
              end
              else
              begin
                FbDecodeDate(Value, K, M, Wd);
                if Count = 1 then Add := IntToStr(Wd) else Add := Pad2(Wd);
                DoAdd := True;
              end;
            end
            else if (Cur = 'd') and ((Count = 3) or (Count = 4)) then
            begin
              Wd := FbWeekday(Value, 1);
              Add := DAYS_LONG[Wd];
              if Count = 3 then Add := Copy(Add, 1, 3);
              Inc(I, Count - 1);
              if not DoOutput then Inc(Info.LengthMin, Length(Add)) else DoAdd := True;
            end
            else if ((Cur = 'm') or (Cur = 'n')) and (Count <= 2) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then
              begin
                Inc(Info.LengthMin, Count);
                if Count = 1 then Inc(Info.LengthOpt);
              end
              else
              begin
                if Count = 1 then Add := IntToStr(MinuteOf(Value)) else Add := Pad2(MinuteOf(Value));
                DoAdd := True;
              end;
            end
            else if ((Cur = 'h') or (Cur = 'H')) and (Count <= 2) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then
              begin
                Inc(Info.LengthMin, Count);
                if Count = 1 then Inc(Info.LengthOpt);
              end
              else
              begin
                Hour := HourOf(Value);
                if Info.HasAmPm and (Cur = 'h') then
                begin
                  if Hour > 12 then Dec(Hour, 12)
                  else if Hour = 0 then Inc(Hour, 12);
                end;
                if Count = 1 then Add := IntToStr(Hour) else Add := Pad2(Hour);
                DoAdd := True;
              end;
              DidHour := True;
            end
            else if (Cur = 's') and (Count <= 2) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then
              begin
                Inc(Info.LengthMin, Count);
                if Count = 1 then Inc(Info.LengthOpt);
              end
              else
              begin
                if Count = 1 then Add := IntToStr(SecondOf(Value)) else Add := Pad2(SecondOf(Value));
                DoAdd := True;
              end;
            end
            else if (Cur = 'M') and (Count <= 2) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then
              begin
                Inc(Info.LengthMin, Count);
                if Count = 1 then Inc(Info.LengthOpt);
              end
              else
              begin
                FbDecodeDate(Value, K, M, Wd);
                if Count = 1 then Add := IntToStr(M) else Add := Pad2(M);
                DoAdd := True;
              end;
            end
            else if (Cur = 'M') and ((Count = 3) or (Count = 4)) then
            begin
              FbDecodeDate(Value, K, M, Wd);
              Add := MONTHS_LONG[M];
              if Count = 3 then Add := Copy(Add, 1, 3);
              Inc(I, Count - 1);
              if not DoOutput then Inc(Info.LengthMin, Length(Add)) else DoAdd := True;
            end
            else if (Cur = 'y') and (Count < 3) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then Inc(Info.LengthMin, 2)
              else
              begin
                FbDecodeDate(Value, K, M, Wd);
                Add := Pad2(K mod 100);
                DoAdd := True;
              end;
            end
            else if (Cur = 'y') and (Count = 4) then
            begin
              Inc(I, Count - 1);
              if not DoOutput then Inc(Info.LengthMin, 4)
              else
              begin
                FbDecodeDate(Value, K, M, Wd);
                Add := Format('%.4d', [K]);
                DoAdd := True;
              end;
            end
            else if not DoOutput then Inc(Info.LengthMin)
            else DoAdd := True;
          end
          else if not DoOutput then Inc(Info.LengthMin)
          else DoAdd := True;
        '/':
          if not DoOutput then Inc(Info.LengthMin)
          else
          begin
            if Info.MaskType = mtDateTime then Add := DATE_SEP;
            DoAdd := True;
          end;
        ':':
          if not DoOutput then Inc(Info.LengthMin)
          else
          begin
            if Info.MaskType = mtDateTime then Add := TIME_SEP;
            DoAdd := True;
          end;
        'a', 'A':
          if (Info.MaskType = mtDateTime) and
             (SameText(Copy(Mask, I + 1, 5), 'AM/PM') or SameText(Copy(Mask, I + 1, 3), 'A/P')) then
          begin
            AmPmSmall := At(I + 1) = '/';
            if not DoOutput then
              Info.HasAmPm := True
            else
            begin
              if AmPmSmall then K := 1 else K := 2;
              if HourOf(Value) >= 12 then Add := Copy(Mask, I + 1 + K + 1, K)
              else Add := Copy(Mask, I + 1, K);
              DoAdd := True;
            end;
            if AmPmSmall then Inc(I, 2) else Inc(I, 4);
          end
          else if not DoOutput then Inc(Info.LengthMin)
          else DoAdd := True;
        '"':
          DoString := True;
      else
        if not DoOutput then Inc(Info.LengthMin) else DoAdd := True;
      end;
      end;
    end;

    if LastWasComma and ((Cur <> ',') or (I = MaskLen - 1)) then
    begin
      if (not DoOutput) and (not WasKDiv) then Info.HasThousandSep := True;
      LastWasComma := False;
      WasKDiv := False;
    end;
    if DoAdd then
    begin
      DoAdd := False;
      Output := Output + Add;
    end;
    Inc(I);
  end;

  if not DoOutput then
  begin
    if (not Info.HasDecimalPoint) and (Info.NumDigitsOmit <> 0) then Inc(Info.NumDigitsOmit, 3);
    if Info.HasThousandSep then Inc(Info.LengthMin, (Info.NumDigitsFix - 1) div 3);
    if LenFix > Info.NumDigitsFix then Inc(Info.LengthMin, LenFix - Info.NumDigitsFix);
    if Info.ExpDigits < 5 then Inc(Info.LengthOpt, 5 - Info.ExpDigits);
    if not Info.HasSign then Inc(Info.LengthMin);
  end;
end;

function FbFormat(Value: Double; const Mask: string): string;
begin
  Result := FormatWith(Value, Mask);
end;

end.
