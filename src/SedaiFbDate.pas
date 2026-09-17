{
  SedaiFbDate - the date arithmetic of FreeBASIC's datetime.bi, as fbc's runtime answers it

  Copyright (C) 2026 Maurizio Cammalleri
  Released under GNU GPL v3 (see LICENCE.md)

  ⛔ WHY THIS UNIT EXISTS (17 Sep 2026, the `datetime` library deck). The VM answered DatePart, DateDiff,
  DateAdd and Weekday with FPC's DateUtils and its own rules, and they differ from fbc where it is least
  visible:
    - Weekday / DatePart / DateDiff take the FIRST DAY OF THE WEEK and of the YEAR; both were dropped;
    - DateDiff("w") counts WEEKDAY crossings, not whole weeks of days (760 against fbc's 108);
    - DateDiff("h"/"n"/"s") floors the day part of the difference, so it is not symmetric
      (-18217 against 18216 for the same two moments), and we rounded;
    - an interval is matched EXACTLY ("YYYY" is not an interval: DatePart answers 0), and we lower-cased
      and defaulted to days.
  ⭐ The rules below follow fbc 1.10.1's rtlib (time_datepart.c, time_datediff.c, time_dateadd.c,
  time_week.c, time_decodeserdate.c), read and re-written, and every one is measured by the deck.
  ⚠️ The serial is a Double with day 0 = 30 Dec 1899, the same epoch as FPC's TDateTime.
}
unit SedaiFbDate;

{$mode objfpc}{$H+}

interface

const
  FBIV_INVALID = -1;
  FBIV_YEAR = 0; FBIV_QUARTER = 1; FBIV_MONTH = 2; FBIV_DAYOFYEAR = 3; FBIV_DAY = 4;
  FBIV_WEEKDAY = 5; FBIV_WEEKOFYEAR = 6; FBIV_HOUR = 7; FBIV_MINUTE = 8; FBIV_SECOND = 9;

// The interval code of a string, matched exactly as fbc matches it.
function FbIntervalCode(const S: string): Integer;
// "iv" or "iv"#0"<fdow>,<fdoy>": the SSA appends the two optional arguments when the program wrote them.
procedure FbSplitIntervalArgs(const S: string; out Iv: Integer; out FDoW, FDoY: Integer);
function FbWeekday(Serial: Double; FirstDayOfWeek: Integer): Integer;
procedure FbDecodeDate(Serial: Double; out Y, M, D: Integer);
procedure FbDecodeTime(Serial: Double; out H, N, S: Integer);
// ...and with the QB quirk, the form Hour/Minute/Second (and Format) use: a negative fraction of day 0 is mirrored.
procedure FbDecodeTimeQB(Serial: Double; out H, N, S: Integer);
function FbDateSerial(Y, M, D: Integer): Integer;
function FbDatePart(Iv: Integer; Serial: Double; FDoW, FDoY: Integer): Integer;
function FbDateDiff(Iv: Integer; S1, S2: Double; FDoW, FDoY: Integer): Int64;
function FbDateAdd(Iv: Integer; Number, Serial: Double): Double;

implementation

uses
  SysUtils, Math;

const
  // TYPED, not a literal: dividing by a literal lets the optimiser multiply by an inexact reciprocal
  // (see bcTimeSerial in SedaiBytecodeVM).
  SECS_PER_DAY: Double = 86400.0;
  HOURS_PER_DAY: Double = 24.0;
  SIXTY: Double = 60.0;

function FbIntervalCode(const S: string): Integer;
begin
  if S = 'yyyy' then Result := FBIV_YEAR
  else if S = 'q' then Result := FBIV_QUARTER
  else if S = 'm' then Result := FBIV_MONTH
  else if S = 'y' then Result := FBIV_DAYOFYEAR
  else if S = 'd' then Result := FBIV_DAY
  else if S = 'w' then Result := FBIV_WEEKDAY
  else if S = 'ww' then Result := FBIV_WEEKOFYEAR
  else if S = 'h' then Result := FBIV_HOUR
  else if S = 'n' then Result := FBIV_MINUTE
  else if S = 's' then Result := FBIV_SECOND
  else Result := FBIV_INVALID;
end;

procedure FbSplitIntervalArgs(const S: string; out Iv: Integer; out FDoW, FDoY: Integer);
var
  P, C: Integer;
  Rest: string;
begin
  FDoW := 0; FDoY := 0;
  P := Pos(#0, S);
  if P = 0 then begin Iv := FbIntervalCode(S); Exit; end;
  Iv := FbIntervalCode(Copy(S, 1, P - 1));
  Rest := Copy(S, P + 1, MaxInt);
  C := Pos(',', Rest);
  if C = 0 then Exit;
  FDoW := StrToIntDef(Trim(Copy(Rest, 1, C - 1)), 0);
  FDoY := StrToIntDef(Trim(Copy(Rest, C + 1, MaxInt)), 0);
end;

function IsLeap(Y: Integer): Boolean;
begin
  if Y mod 400 = 0 then Exit(True);
  if Y mod 100 = 0 then Exit(False);
  Result := (Y and 3) = 0;
end;

function DaysInYear(Y: Integer): Integer;
begin
  if IsLeap(Y) then Result := 366 else Result := 365;
end;

function DaysInMonth(M, Y: Integer): Integer;
const
  DAYS: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DAYS[M];
  if (M = 2) and IsLeap(Y) then Inc(Result);
end;

// Whole days from 1 Jan 1900 to 1 Jan Y (negative before 1900): 400-year cycles are 146097 days.
function DaysBeforeYear(Y: Integer): Int64;
var
  Cycles, Yr: Integer;
begin
  Result := 0;
  Yr := 1900;
  Cycles := (Y - Yr) div 400;
  Result := Int64(Cycles) * 146097;
  Inc(Yr, Cycles * 400);
  while Yr < Y do begin Inc(Result, DaysInYear(Yr)); Inc(Yr); end;
  while Yr > Y do begin Dec(Yr); Dec(Result, DaysInYear(Yr)); end;
end;

procedure FbDecodeDate(Serial: Double; out Y, M, D: Integer);
var
  Days, T: Int64;
begin
  // fb_hDateDecodeSerial: day 2 is 1 Jan 1900.
  Days := Trunc(Floor64(Serial)) - 2;
  Y := 1900 + Integer(Days div 146097) * 400;
  Days := Days mod 146097;
  while Days < 0 do begin Dec(Y); Inc(Days, DaysInYear(Y)); end;
  T := DaysInYear(Y);
  while Days >= T do begin Dec(Days, T); Inc(Y); T := DaysInYear(Y); end;
  M := 1;
  T := DaysInMonth(M, Y);
  while Days >= T do begin Dec(Days, T); Inc(M); T := DaysInMonth(M, Y); end;
  D := 1 + Integer(Days);
end;

procedure FbDecodeTime(Serial: Double; out H, N, S: Integer);
// fb_hTimeDecodeSerial without the QB quirk (the form DatePart and DateDiff use).
var
  F: Double;
begin
  F := Serial - Int(Serial);
  if F < 0 then F := F + 1.0;
  F := F + 0.000000001;
  F := F * 24.0; H := Trunc(F); F := F - H;
  F := F * 60.0; N := Trunc(F); F := F - N;
  F := F * 60.0; S := Trunc(F);
end;

procedure FbDecodeTimeQB(Serial: Double; out H, N, S: Integer);
var
  F, Fix: Double;
begin
  Fix := Int(Serial);
  F := Serial - Fix;
  if F < 0 then
  begin
    if Fix = 0.0 then F := -F else F := F + 1.0;
  end;
  F := F + 0.000000001;
  F := F * 24.0; H := Trunc(F); F := F - H;
  F := F * 60.0; N := Trunc(F); F := F - N;
  F := F * 60.0; S := Trunc(F);
end;

procedure NormalizeDate(var D, M, Y: Integer);
var
  SubMonths, SubYears, SubDays, Dom: Integer;
begin
  if M < 1 then
  begin
    SubMonths := -M + 1;
    SubYears := (SubMonths + 11) div 12;
    Dec(Y, SubYears);
    M := SubYears * 12 - SubMonths + 1;
  end
  else
  begin
    Dec(M);
    Inc(Y, M div 12);
    M := M mod 12;
    Inc(M);
  end;
  if D < 1 then
  begin
    SubDays := -D + 1;
    while SubDays > 0 do
    begin
      Dec(M);
      if M = 0 then begin M := 12; Dec(Y); end;
      Dom := DaysInMonth(M, Y);
      if SubDays > Dom then Dec(SubDays, Dom)
      else begin D := Dom - SubDays + 1; SubDays := 0; end;
    end;
  end
  else
  begin
    Dom := DaysInMonth(M, Y);
    while D > Dom do
    begin
      Dec(D, Dom);
      Inc(M);
      if M = 13 then begin M := 1; Inc(Y); end;
      Dom := DaysInMonth(M, Y);
    end;
  end;
end;

function FbDateSerial(Y, M, D: Integer): Integer;
var
  I: Integer;
  R: Int64;
begin
  NormalizeDate(D, M, Y);
  R := 2 + DaysBeforeYear(Y);
  for I := 1 to M - 1 do Inc(R, DaysInMonth(I, Y));
  Inc(R, D - 1);
  Result := Integer(R);
end;

function FbWeekday(Serial: Double; FirstDayOfWeek: Integer): Integer;
begin
  // C's "%" truncates toward zero, and so does Pascal's "mod": the adjustment below is fb's own.
  // ...and "(int)" of a serial out of 32-bit range is x86-64's integer-indefinite value, INT_MIN: fbc's weekday of
  // 1e300 is Friday because of it.
  if (Floor64(Serial) - 1 > High(LongInt)) or (Floor64(Serial) - 1 < Low(LongInt)) or IsNan(Serial) then
    Result := (Low(LongInt) mod 7) + 1
  else
    Result := (LongInt(Floor64(Serial) - 1) mod 7) + 1;
  if FirstDayOfWeek = 0 then FirstDayOfWeek := 1;   // "system": Sunday, as fb's runtime has it
  Dec(Result, FirstDayOfWeek - 1);
  if Result < 1 then Inc(Result, 7)
  else if Result > 7 then Dec(Result, 7);
end;

function FirstWeekOfYear(Year, FDoY, FDoW: Integer): Double;
var
  Y, M, D, Remaining: Integer;
  YearBegin, WeekBegin: Double;
begin
  if FDoY = 0 then FDoY := 1;        // "system": 1 January
  YearBegin := FbDateSerial(Year, 1, 1);
  // The beginning of the week holding 1 January.
  WeekBegin := YearBegin - (FbWeekday(YearBegin, FDoW) - 1);
  FbDecodeDate(WeekBegin, Y, M, D);
  WeekBegin := FbDateSerial(Y, M, D);
  Remaining := Trunc((WeekBegin + 7.0) - YearBegin);
  case FDoY of
    2: if Remaining < 4 then WeekBegin := WeekBegin + 7.0;   // first four-day week
    3: if Remaining < 7 then WeekBegin := WeekBegin + 7.0;   // first full week
  end;
  Result := WeekBegin;
end;

function WeekOfYear(RefYear: Integer; Serial: Double; FDoY, FDoW: Integer): Integer;
var
  D: Double;
begin
  D := Floor64(Serial - FirstWeekOfYear(RefYear, FDoY, FDoW));
  // fb_hSign answers -1 for a negative value and +1 for everything else, ZERO INCLUDED: Math.Sign would give 0.
  if D < 0 then Result := Trunc(D / 7.0 - 1.0) else Result := Trunc(D / 7.0 + 1.0);
end;

function FbDatePart(Iv: Integer; Serial: Double; FDoW, FDoY: Integer): Integer;
var
  Y, M, D, H, N, S, I: Integer;
begin
  Result := 0;
  case Iv of
    FBIV_YEAR:    begin FbDecodeDate(Serial, Y, M, D); Result := Y; end;
    FBIV_QUARTER: begin FbDecodeDate(Serial, Y, M, D); Result := (M - 1) div 3 + 1; end;
    FBIV_MONTH:   begin FbDecodeDate(Serial, Y, M, D); Result := M; end;
    FBIV_DAYOFYEAR:
      begin
        FbDecodeDate(Serial, Y, M, D);
        Result := D;
        for I := 1 to M - 1 do Inc(Result, DaysInMonth(I, Y));
      end;
    FBIV_DAY:     begin FbDecodeDate(Serial, Y, M, D); Result := D; end;
    FBIV_WEEKDAY: Result := FbWeekday(Serial, FDoW);
    FBIV_WEEKOFYEAR:
      begin
        FbDecodeDate(Serial, Y, M, D);
        Result := WeekOfYear(Y, Serial, FDoY, FDoW);
        if Result < 0 then Result := WeekOfYear(Y - 1, Serial, FDoY, FDoW);
      end;
    FBIV_HOUR:    begin FbDecodeTime(Serial, H, N, S); Result := H; end;
    FBIV_MINUTE:  begin FbDecodeTime(Serial, H, N, S); Result := N; end;
    FBIV_SECOND:  begin FbDecodeTime(Serial, H, N, S); Result := S; end;
  end;
end;

function FbDateDiff(Iv: Integer; S1, S2: Double; FDoW, FDoY: Integer): Int64;
var
  Y1, M1, D1, Y2, M2, D2, H, N, S, Week: Integer;
  Serial, Tmp, Days: Double;
  AddValue: Integer;
begin
  Result := 0;
  case Iv of
    FBIV_YEAR:
      begin
        FbDecodeDate(S1, Y1, M1, D1); FbDecodeDate(S2, Y2, M2, D2);
        Result := Y2 - Y1;
      end;
    FBIV_QUARTER, FBIV_MONTH:
      begin
        FbDecodeDate(S1, Y1, M1, D1); FbDecodeDate(S2, Y2, M2, D2);
        Result := (M2 - M1) + (Y2 - Y1) * 12;
        if Iv = FBIV_QUARTER then Result := Result div 3;
      end;
    FBIV_DAYOFYEAR, FBIV_DAY:
      Result := Trunc(Floor64(S2) - Floor64(S1));
    FBIV_WEEKDAY, FBIV_WEEKOFYEAR:
      begin
        // ⚠️ Both weeks are counted from the FIRST serial's year, as fbc does.
        FbDecodeDate(S1, Y1, M1, D1);
        Week := WeekOfYear(Y1, S1, FDoY, FDoW);
        Result := WeekOfYear(Y1, S2, FDoY, FDoW);
        if Week > 0 then Dec(Week);
        if Result > 0 then Dec(Result);
        Dec(Result, Week);
        if Iv = FBIV_WEEKDAY then
        begin
          if S1 > S2 then
          begin
            Tmp := S1; S1 := S2; S2 := Tmp;
            AddValue := 1;
          end
          else
            AddValue := -1;
          if FbWeekday(S1, FDoW) > FbWeekday(S2, FDoW) then Inc(Result, AddValue);
        end;
      end;
    FBIV_HOUR, FBIV_MINUTE, FBIV_SECOND:
      begin
        // The DAY part of the difference is FLOORED and the time part read from its fraction, so a negative
        // difference is not the mirror of the positive one - fbc's answer, kept.
        // ⛔ Floor answers an INTEGER, and an integer times an unsuffixed real literal is computed in SINGLE by
        // FPC: 65 577 602 seconds came out 65 577 600. Days is a Double, and so is everything after it.
        Serial := S2 - S1;
        FbDecodeTime(Serial, H, N, S);
        Days := Floor64(Serial);
        case Iv of
          FBIV_HOUR:   Result := Trunc(H + Days * HOURS_PER_DAY);
          FBIV_MINUTE: Result := Trunc(N + (H + Days * HOURS_PER_DAY) * SIXTY);
        else
          Result := Trunc(S + (N + (H + Days * HOURS_PER_DAY) * SIXTY) * SIXTY);
        end;
      end;
  end;
end;

function FbDateAdd(Iv: Integer; Number, Serial: Double): Double;
var
  Y, M, D, H, N, S, Value, Carry, Dim_: Integer;
begin
  Value := Trunc(Number);
  FbDecodeTime(Serial, H, N, S);
  FbDecodeDate(Serial, Y, M, D);
  case Iv of
    FBIV_YEAR: Inc(Y, Value);
    FBIV_QUARTER: Inc(M, Value * 3);
    FBIV_MONTH: Inc(M, Value);
    FBIV_DAYOFYEAR, FBIV_DAY, FBIV_WEEKDAY: Inc(D, Value);
    FBIV_WEEKOFYEAR: Inc(D, Value * 7);
    FBIV_HOUR: Inc(H, Value);
    FBIV_MINUTE: Inc(N, Value);
    FBIV_SECOND: Inc(S, Value);
  end;
  if Iv in [FBIV_YEAR, FBIV_QUARTER, FBIV_MONTH] then
  begin
    // A month past the end wraps into the year; a day past the end SATURATES (31 Jan + 1 month = 29 Feb).
    if M < 1 then Carry := (M - 12) div 12 else Carry := (M - 1) div 12;
    Inc(Y, Carry);
    Dec(M, Carry * 12);
    Dim_ := DaysInMonth(M, Y);
    if D > Dim_ then D := Dim_;
  end;
  Result := FbDateSerial(Y, M, D) + (H * 3600 + N * 60 + S) / SECS_PER_DAY;
end;

end.
