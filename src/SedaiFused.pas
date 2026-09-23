unit SedaiFused;

// =====================================================================================================
// THE FUSED EXECUTABLE - a program that IS an executable (DIVERGENZE 573, owner 23 Sep 2026).
//
//   ┌──────────────────────────────┐
//   │ the sb binary, untouched     │  ELF / PE loaders never read bytes past the image
//   ├──────────────────────────────┤
//   │ the .basc, byte for byte     │  what sbc writes today
//   ├──────────────────────────────┤
//   │ TRAILER, 32 fixed bytes      │  TFusedTrailer below, the magic LAST
//   └──────────────────────────────┘
//
// ⭐ WHY: a C library that looks for its files next to the EXECUTABLE asks the kernel (/proc/self/exe,
// GetModuleFileName) - under `sb prog.bas` the answer is the interpreter. In a fused file the answer IS the
// program, in the program's own directory, on every platform, with nothing intercepted. It is what LÖVE
// ("fused games"), Node's Single Executable Applications, `deno compile` and PyInstaller do.
//
// ⛔ The .basc and the runtime that runs it travel together, so their versions always agree by construction;
// the trailer carries a version of its own only for the trailer. The payload is checked with a CRC32: a file
// that `strip` truncated, or a download cut short, is refused with a message that says so, never run.
// Design and decisions: job/markdown/ESEGUIBILE-FUSO.md.
// =====================================================================================================

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  FUSED_TRAILER_VERSION = 1;
  FUSED_FLAG_JIT  = 1;   // run with the loop JIT, as `sb --jit`
  FUSED_FLAG_HOME = 2;   // run in the program's directory, as `sb --home`

type
  TFusedTrailer = packed record
    PayloadLen: QWord;                  // bytes of the .basc, which ends where the trailer starts
    Crc: LongWord;                      // CRC32 of those bytes
    Flags: LongWord;                    // FUSED_FLAG_*
    Version: LongWord;                  // FUSED_TRAILER_VERSION
    Magic: array[0..7] of AnsiChar;     // 'SBFUSED'#0 - the last-but-four bytes of the file
    Reserved: LongWord;
  end;

{ The path of the running executable as the kernel knows it - never argv[0], which the caller chooses. }
function SelfExecutablePath: string;

{ True when AFile ends with a fused trailer. Then APayload holds the .basc (a fresh stream, positioned at 0,
  owned by the caller) and AFlags its FUSED_FLAG_*. A file that carries the magic but whose payload is
  damaged raises EFusedError naming what is wrong: it must never fall back to being a plain `sb`. }
function ReadFusedPayload(const AFile: string; out APayload: TMemoryStream; out AFlags: LongWord): Boolean;

{ Writes ARuntime + ABasc + trailer to AOut and makes it executable. }
procedure WriteFusedExecutable(const ARuntime, ABasc, AOut: string; AFlags: LongWord);

function FusedCrc32(const Buf; Len: SizeInt): LongWord;

type
  EFusedError = class(Exception);

implementation

{$IFDEF UNIX}
uses BaseUnix;
{$ENDIF}
{$IFDEF WINDOWS}
uses Windows;
{$ENDIF}

const
  FUSED_MAGIC: array[0..7] of AnsiChar = ('S','B','F','U','S','E','D',#0);

var
  CrcTable: array[0..255] of LongWord;
  CrcReady: Boolean = False;

procedure BuildCrcTable;
var
  i, k: Integer;
  c: LongWord;
begin
  for i := 0 to 255 do
  begin
    c := LongWord(i);
    for k := 0 to 7 do
      if (c and 1) <> 0 then c := $EDB88320 xor (c shr 1) else c := c shr 1;
    CrcTable[i] := c;
  end;
  CrcReady := True;
end;

function FusedCrc32(const Buf; Len: SizeInt): LongWord;
// The IEEE CRC32 (the one of zip and gzip), so `crc32` from any tool checks a payload cut out by hand.
var
  P: PByte;
  i: SizeInt;
begin
  if not CrcReady then BuildCrcTable;
  Result := $FFFFFFFF;
  P := @Buf;
  for i := 0 to Len - 1 do
    Result := CrcTable[(Result xor P[i]) and $FF] xor (Result shr 8);
  Result := not Result;
end;

function SelfExecutablePath: string;
{$IFDEF WINDOWS}
var
  W: array[0..32767] of WideChar;
  N: DWORD;
  WS: UnicodeString;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := fpReadLink('/proc/self/exe');   // a system call: FPC does not go through libc for this
  {$ELSE}
  {$IFDEF WINDOWS}
  N := GetModuleFileNameW(0, @W[0], Length(W));
  if N > 0 then
  begin
    SetString(WS, PWideChar(@W[0]), N);
    Result := UTF8Encode(WS);
  end
  else Result := '';
  {$ELSE}
  Result := '';
  {$ENDIF}
  {$ENDIF}
  if Result = '' then Result := ExpandFileName(ParamStr(0));
end;

function ReadFusedPayload(const AFile: string; out APayload: TMemoryStream; out AFlags: LongWord): Boolean;
var
  F: TFileStream;
  T: TFusedTrailer;
  Size: Int64;
begin
  Result := False;
  APayload := nil;
  AFlags := 0;
  if (AFile = '') or not FileExists(AFile) then Exit;
  try
    F := TFileStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  except
    Exit;   // an executable we cannot read is not a fused one: run as a plain sb
  end;
  try
    Size := F.Size;
    if Size < SizeOf(T) then Exit;
    F.Position := Size - SizeOf(T);
    F.ReadBuffer(T, SizeOf(T));
    if not CompareMem(@T.Magic[0], @FUSED_MAGIC[0], SizeOf(FUSED_MAGIC)) then Exit;
    // ⛔ From here on the file SAYS it is fused: every defect is an error, never a quiet plain run.
    if T.Version <> FUSED_TRAILER_VERSION then
      raise EFusedError.CreateFmt('fused program %s: trailer version %d, this runtime reads %d',
        [AFile, T.Version, FUSED_TRAILER_VERSION]);
    if (T.PayloadLen = 0) or (Int64(T.PayloadLen) > Size - SizeOf(T)) then
      raise EFusedError.CreateFmt('fused program %s: the program inside is damaged (length %d in a file of %d bytes)'
        + ' - was the file truncated or stripped?', [AFile, Int64(T.PayloadLen), Size]);
    APayload := TMemoryStream.Create;
    try
      F.Position := Size - SizeOf(T) - Int64(T.PayloadLen);
      APayload.CopyFrom(F, Int64(T.PayloadLen));
      if FusedCrc32(APayload.Memory^, APayload.Size) <> T.Crc then
        raise EFusedError.CreateFmt('fused program %s: the program inside is damaged (wrong CRC)'
          + ' - was the file truncated or stripped?', [AFile]);
      APayload.Position := 0;
    except
      FreeAndNil(APayload);
      raise;
    end;
    AFlags := T.Flags;
    Result := True;
  finally
    F.Free;
  end;
end;

procedure WriteFusedExecutable(const ARuntime, ABasc, AOut: string; AFlags: LongWord);
var
  R, B: TMemoryStream;
  O: TFileStream;
  T: TFusedTrailer;
  Dummy: TMemoryStream;
  DummyFlags: LongWord;
begin
  R := TMemoryStream.Create;
  B := TMemoryStream.Create;
  try
    R.LoadFromFile(ARuntime);
    B.LoadFromFile(ABasc);
    // ⛔ A runtime that is ITSELF fused would nest two programs; the loader reads only the last trailer, so
    // the result would run - the wrong half of it made of a program nobody asked for. Refused.
    if ReadFusedPayload(ARuntime, Dummy, DummyFlags) then
    begin
      Dummy.Free;
      raise EFusedError.CreateFmt('%s is already a fused program, not the sb runtime', [ARuntime]);
    end;
    if (B.Size < 4) or (PAnsiChar(B.Memory)[0] <> 'B') or (PAnsiChar(B.Memory)[1] <> 'A') or
       (PAnsiChar(B.Memory)[2] <> 'S') or (PAnsiChar(B.Memory)[3] <> 'C') then
      raise EFusedError.CreateFmt('%s is not a .basc file', [ABasc]);
    FillChar(T, SizeOf(T), 0);
    T.PayloadLen := B.Size;
    T.Crc := FusedCrc32(B.Memory^, B.Size);
    T.Flags := AFlags;
    T.Version := FUSED_TRAILER_VERSION;
    Move(FUSED_MAGIC[0], T.Magic[0], SizeOf(FUSED_MAGIC));
    O := TFileStream.Create(AOut, fmCreate);
    try
      O.WriteBuffer(R.Memory^, R.Size);
      O.WriteBuffer(B.Memory^, B.Size);
      O.WriteBuffer(T, SizeOf(T));
    finally
      O.Free;
    end;
    {$IFDEF UNIX}
    fpChmod(AOut, &755);
    {$ENDIF}
  finally
    R.Free;
    B.Free;
  end;
end;

end.
