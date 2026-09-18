unit SedaiFbRnd;
{ RND, RND32 and RANDOMIZE as fbc's runtime answers them (DIVERGENZE 542).

  A line-by-line reading of libfb's src/rtlib/math_rnd.c (fbc 1.10.1) and of hRnd_FillFAST32 in fb_math.h: the five
  built-in generators FB.FB_RND_ALGORITHMS names - CRT (the C library's rand), FAST (the Numerical Recipes LCG),
  MTWIST (MT19937 with its state filled by FAST, NOT by init_genrand), QB (QBasic's 24-bit LCG) and REAL (the
  system's entropy, MTWIST if it cannot be read). In the fb dialect AUTO means MTWIST, and a program that draws
  before any RANDOMIZE gets RANDOMIZE 0 - so an unseeded program gives the SAME numbers on every run, as it does
  under fbc.

  ⛔ Before this unit the engine had FPC's Random: an MT19937 too, but seeded by init_genrand, so no seed ever gave
  fbc's sequence, and the algorithm argument of RANDOMIZE was read and thrown away. fbprng.bi's deck found it.
  ⚠ The state is ONE for the process, as fbc's ctx is; fbc takes a lock around it in a multithreaded program, and
  so does this unit. Only the MODERN dialect comes here - Commodore RND keeps its own rules in the VM.

  Copyright (c) 2025 Maurizio Cammalleri
  Released under GNU GPL v3 }

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

const
  FB_RND_AUTO   = 0;
  FB_RND_CRT    = 1;
  FB_RND_FAST   = 2;
  FB_RND_MTWIST = 3;
  FB_RND_QB     = 4;
  FB_RND_REAL   = 5;

procedure FbRandomize(Seed: Double; Algorithm: Int64);   // Seed = -1.0: from the clock, as fbc
function FbRnd(N: Double): Double;                       // RND(n): n = 0 answers the last number again
function FbRnd32: LongWord;                              // RND32, the raw 32-bit draw

implementation

uses
  SysUtils
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

const
  MAX_STATE    = 624;
  PERIOD       = 397;
  INITIAL_SEED = 327680;

{$IFDEF WINDOWS}
  CRT_LIB  = 'msvcrt';
  RAND_MAX = 32767;
{$ELSE}
  CRT_LIB  = 'c';
  RAND_MAX = 2147483647;
{$ENDIF}

function c_rand: LongInt; cdecl; external CRT_LIB name 'rand';
procedure c_srand(Seed: LongWord); cdecl; external CRT_LIB name 'srand';

var
  Lock: TRTLCriticalSection;
  Algo: Integer = -1;                    // -1: nothing drawn and no RANDOMIZE yet (fbc's hRnd_Startup)
  ISeed32: LongWord;
  State32: array[0..MAX_STATE - 1] of LongWord;
  Index32: Integer = -1;                 // -1 is fbc's NULL index pointer
  LastNum: Double = 0.0;                 // what RND(0) answers for CRT, MTWIST and REAL; never reset

function Fast32(X: LongWord): LongWord; inline;
begin
  {$PUSH}{$Q-}{$R-}
  Result := X * 1664525 + 1013904223;
  {$POP}
end;

procedure FillFast32(Seed: LongWord);
var
  i: Integer;
begin
  State32[0] := Seed;
  for i := 1 to MAX_STATE - 1 do State32[i] := Fast32(State32[i - 1]);
end;

procedure InitMTwist(Seed: LongWord);
begin
  Algo := FB_RND_MTWIST;
  Index32 := MAX_STATE;
  FillFast32(Seed);
end;

function MTwist32: LongWord;
const
  XorMask: array[0..1] of LongWord = (0, $9908B0DF);
var
  i: Integer;
  v: LongWord;
begin
  if Index32 < 0 then InitMTwist(INITIAL_SEED);
  if Index32 >= MAX_STATE then
  begin
    for i := 0 to MAX_STATE - PERIOD - 1 do
    begin
      v := (State32[i] and $80000000) or (State32[i + 1] and $7FFFFFFF);
      State32[i] := State32[i + PERIOD] xor (v shr 1) xor XorMask[v and 1];
    end;
    for i := MAX_STATE - PERIOD to MAX_STATE - 2 do
    begin
      v := (State32[i] and $80000000) or (State32[i + 1] and $7FFFFFFF);
      State32[i] := State32[i + PERIOD - MAX_STATE] xor (v shr 1) xor XorMask[v and 1];
    end;
    v := (State32[MAX_STATE - 1] and $80000000) or (State32[0] and $7FFFFFFF);
    State32[MAX_STATE - 1] := State32[PERIOD - 1] xor (v shr 1) xor XorMask[v and 1];
    Index32 := 0;
  end;
  v := State32[Index32];
  Inc(Index32);
  v := v xor (v shr 11);
  v := v xor ((v shl 7) and $9D2C5680);
  v := v xor ((v shl 15) and $EFC60000);
  v := v xor (v shr 18);
  Result := v;
end;

function Qb32: LongWord;
begin
  {$PUSH}{$Q-}{$R-}
  ISeed32 := ((ISeed32 * $FD43FD) + $C39EC3) and $FFFFFF;
  {$POP}
  Result := ISeed32;
end;

// FB_RND_REAL: the system's entropy, 624 words at a time; when it cannot be read, fbc falls back to MTWIST.
function RefillReal: Boolean;
{$IFDEF UNIX}
var
  fd: cint;
begin
  Result := False;
  fd := FpOpen('/dev/urandom', O_RDONLY);
  if fd < 0 then Exit;
  Result := FpRead(fd, State32[0], SizeOf(State32)) = SizeOf(State32);
  FpClose(fd);
  if Result then Index32 := 0;
end;
{$ELSE}
var
  i: Integer;
begin
  // No CryptGenRandom binding here: the documented fallback of fbc itself.
  Randomize;
  for i := 0 to MAX_STATE - 1 do State32[i] := LongWord(Random($7FFFFFFF)) xor (LongWord(Random(2)) shl 31);
  Index32 := 0;
  Result := True;
end;
{$ENDIF}

procedure RandomizeLocked(Seed: Double; Algorithm: Int64); forward;

function Real32: LongWord;
begin
  if Index32 >= MAX_STATE then
    if not RefillReal then
    begin
      RandomizeLocked(-1.0, FB_RND_MTWIST);
      Exit(MTwist32);
    end;
  Result := State32[Index32];
  Inc(Index32);
end;

function Draw32: LongWord;
begin
  case Algo of
    FB_RND_CRT:  Result := LongWord(c_rand);
    FB_RND_FAST: begin ISeed32 := Fast32(ISeed32); Result := ISeed32; end;
    FB_RND_QB:   Result := Qb32;
    FB_RND_REAL: Result := Real32;
  else           Result := MTwist32;
  end;
end;

// (uint32_t)seed: gcc on x86-64 converts through a 64-bit truncation and keeps the low half.
function SeedU32(Seed: Double): LongWord;
begin
  if (Seed >= -9.2e18) and (Seed <= 9.2e18) then
    Result := LongWord(Int64(Trunc(Seed)) and $FFFFFFFF)
  else
    Result := 0;
end;

procedure RandomizeLocked(Seed: Double; Algorithm: Int64);
var
  Bits: QWord;
  T: Double;
  s: LongWord;
begin
  if Seed = -1.0 then
  begin
    // fbc: the bits of TIMER, low half xor high half - a value that changes more than once a second.
    T := Frac(Now) * 86400.0;
    Move(T, Bits, 8);
    Seed := LongWord(Bits) xor LongWord(Bits shr 32);
  end;
  if Algorithm = FB_RND_AUTO then Algorithm := FB_RND_MTWIST;   // the fb dialect's AUTO
  case Algorithm of
    FB_RND_CRT:
      begin
        Algo := FB_RND_CRT;
        Index32 := -1;
        c_srand(SeedU32(Seed));
        c_rand;                                   // fbc draws one after srand
      end;
    FB_RND_FAST:
      begin
        Algo := FB_RND_FAST;
        Index32 := -1;
        ISeed32 := SeedU32(Seed);
      end;
    FB_RND_QB:
      begin
        Move(Seed, Bits, 8);
        s := LongWord(Bits shr 32);
        s := s xor (s shr 16);
        s := ((s and $FFFF) shl 8) or (ISeed32 and $FF);
        Algo := FB_RND_QB;
        Index32 := -1;
        ISeed32 := s;
      end;
    FB_RND_REAL:
      begin
        Algo := FB_RND_REAL;
        Index32 := MAX_STATE;
        FillFast32(SeedU32(Seed));
      end;
  else
    InitMTwist(SeedU32(Seed));
  end;
end;

procedure Startup;
begin
  if Algo < 0 then RandomizeLocked(0.0, FB_RND_AUTO);   // hStartup, fb dialect: RANDOMIZE 0
end;

procedure FbRandomize(Seed: Double; Algorithm: Int64);
begin
  EnterCriticalSection(Lock);
  try
    RandomizeLocked(Seed, Algorithm);
  finally
    LeaveCriticalSection(Lock);
  end;
end;

function FbRnd(N: Double): Double;
var
  f: Single;
  s: LongWord;
begin
  N := Single(N);                         // "byval n as single" in fbc: 1e-50 is 0 there
  EnterCriticalSection(Lock);
  try
    Startup;
    case Algo of
      FB_RND_FAST:
        if N = 0.0 then Result := ISeed32 / Double(4294967296.0)
        else Result := Draw32 / Double(4294967296.0);
      FB_RND_QB:
        begin
          if N = 0.0 then Result := Single(ISeed32) / Single(16777216.0)
          else
          begin
            if N < 0.0 then
            begin
              f := N;                             // the argument travels as a SINGLE in fbc
              Move(f, s, 4);
              {$PUSH}{$Q-}{$R-}
              ISeed32 := s + (s shr 24);
              {$POP}
            end;
            Result := Single(Qb32) / Single(16777216.0);
          end;
        end;
      FB_RND_CRT:
        if N = 0.0 then Result := LastNum
        else Result := Draw32 * (Double(1.0) / (Double(RAND_MAX) + 1.0));
    else
      if N = 0.0 then Result := LastNum
      else Result := Draw32 / Double(4294967296.0);
    end;
    LastNum := Result;
  finally
    LeaveCriticalSection(Lock);
  end;
end;

function FbRnd32: LongWord;
begin
  EnterCriticalSection(Lock);
  try
    Startup;
    Result := Draw32;
  finally
    LeaveCriticalSection(Lock);
  end;
end;

initialization
  InitCriticalSection(Lock);
finalization
  DoneCriticalSection(Lock);
end.
