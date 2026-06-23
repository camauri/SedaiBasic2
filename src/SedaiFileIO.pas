unit SedaiFileIO;

{ Headless file-I/O handler for the SedaiBasic VM.

  The VM is decoupled from storage through two callbacks (OnDiskFile / OnFileData,
  see SedaiBytecodeVM). The interactive console (SedaiNewConsole) provides its own
  implementation; this unit provides an equivalent, self-contained handler so the
  CLI VM (sb) — and any other headless host — can do real file I/O.

  Files use BASIC handle numbers 1..15, each backed by a TFileStream. The handler
  understands both the legacy C64/C128 commands (DOPEN/DCLOSE/PRINT#/INPUT#/GET#/
  APPEND/RECORD) and the FreeBASIC additions surfaced via OnFileData query commands
  (LINEINPUT#, EOF, FREEFILE, LOF, LOC, SEEK, SEEKSET, WRITE#). Mode string letters:
  'R' read, 'W' write/truncate, 'A' append, 'B' binary (read+write, no truncate). }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiBytecodeVM;

type
  TVMFileHandler = class
  private
    FFileHandles: array[1..15] of TFileStream;
    FFileModes: array[1..15] of string;
  public
    destructor Destroy; override;
    procedure CloseAll;
    // Wire these to VM.OnDiskFile / VM.OnFileData.
    procedure DiskFile(Sender: TBytecodeVM; const Command: string; Handle: Integer;
      const HandleName, Filename, Mode: string; var ErrorCode: Integer);
    procedure FileData(Sender: TBytecodeVM; const Command: string; Handle: Integer;
      var Data: string; var ErrorCode: Integer);
  end;

implementation

destructor TVMFileHandler.Destroy;
begin
  CloseAll;
  inherited Destroy;
end;

procedure TVMFileHandler.CloseAll;
var
  i: Integer;
begin
  for i := 1 to 15 do
    if Assigned(FFileHandles[i]) then
    begin
      FreeAndNil(FFileHandles[i]);
      FFileModes[i] := '';
    end;
end;

procedure TVMFileHandler.DiskFile(Sender: TBytecodeVM; const Command: string; Handle: Integer;
  const HandleName, Filename, Mode: string; var ErrorCode: Integer);
var
  M: string;
  FileMode: Word;
begin
  ErrorCode := 0;
  if (Handle < 1) or (Handle > 15) then begin ErrorCode := 64; Exit; end;

  if Command = 'DOPEN' then
  begin
    if Assigned(FFileHandles[Handle]) then
    begin
      FreeAndNil(FFileHandles[Handle]);
      FFileModes[Handle] := '';
    end;
    M := UpperCase(Mode);
    if not FileExists(Filename) and
       (Pos('W', M) = 0) and (Pos('A', M) = 0) and (Pos('B', M) = 0) then
    begin
      ErrorCode := 62;  // FILE NOT FOUND (read of a missing file)
      Exit;
    end;
    if Pos('W', M) > 0 then
      FileMode := fmCreate
    else if (Pos('A', M) > 0) or (Pos('B', M) > 0) then
    begin
      if FileExists(Filename) then FileMode := fmOpenReadWrite else FileMode := fmCreate;
    end
    else
      FileMode := fmOpenRead or fmShareDenyNone;
    try
      FFileHandles[Handle] := TFileStream.Create(Filename, FileMode);
      FFileModes[Handle] := M;
      if Pos('A', M) > 0 then FFileHandles[Handle].Seek(0, soEnd);
    except
      on E: EFOpenError do begin ErrorCode := 62; FFileHandles[Handle] := nil; end;
      on E: EFCreateError do begin ErrorCode := 26; FFileHandles[Handle] := nil; end;
      on E: Exception do begin ErrorCode := 70; FFileHandles[Handle] := nil; end;
    end;
  end
  else if Command = 'DCLOSE' then
  begin
    if Assigned(FFileHandles[Handle]) then
    begin
      FreeAndNil(FFileHandles[Handle]);
      FFileModes[Handle] := '';
    end;
  end;
end;

procedure TVMFileHandler.FileData(Sender: TBytecodeVM; const Command: string; Handle: Integer;
  var Data: string; var ErrorCode: Integer);
var
  Ch: Byte;
  Line: string;
  i: Integer;
  FS: TFileStream;
begin
  ErrorCode := 0;

  // FREEFILE: lowest unused handle 1..15 (0 if none). Does not need an open handle.
  if Command = 'FREEFILE' then
  begin
    Data := '0';
    for i := 1 to 15 do
      if not Assigned(FFileHandles[i]) then begin Data := IntToStr(i); Break; end;
    Exit;
  end;

  if (Handle < 1) or (Handle > 15) or (not Assigned(FFileHandles[Handle])) then
  begin
    ErrorCode := 64;  // FILE NOT OPEN
    if (Command = 'EOF') then Data := '-1';   // EOF of a closed file = true
    Exit;
  end;
  FS := FFileHandles[Handle];

  if Command = 'EOF' then
    // FreeBASIC EOF: -1 (true) at/after end of file, 0 otherwise.
    Data := IntToStr(-Ord(FS.Position >= FS.Size))
  else if Command = 'LOF' then
    Data := IntToStr(FS.Size)
  else if Command = 'LOC' then
    Data := IntToStr(FS.Position)
  else if Command = 'SEEK' then
    // SEEK(#n) query: current 1-based byte position (FreeBASIC SEEK is 1-based).
    Data := IntToStr(FS.Position + 1)
  else if Command = 'SEEKSET' then
  begin
    // SEEK #n, pos statement: set the 1-based position.
    try FS.Position := StrToInt64(Data) - 1; except ErrorCode := 63; end;
  end
  else if Command = 'GET#' then
  begin
    if FS.Read(Ch, 1) > 0 then Data := Chr(Ch) else Data := '';
  end
  else if (Command = 'INPUT#') or (Command = 'LINEINPUT#') then
  begin
    if FS.Position >= FS.Size then begin ErrorCode := 62; Data := ''; Exit; end;
    Line := '';
    while FS.Position < FS.Size do
    begin
      FS.Read(Ch, 1);
      if Ch in [10, 13] then
      begin
        if (Ch = 13) and (FS.Position < FS.Size) then
        begin
          FS.Read(Ch, 1);
          if Ch <> 10 then FS.Seek(-1, soCurrent);
        end;
        Break;
      end
      else if (Ch = Ord(',')) and (Command = 'INPUT#') then
        Break   // comma is a field separator for INPUT#, but not for LINE INPUT#
      else
        Line := Line + Chr(Ch);
    end;
    Data := Line;
  end
  else if (Command = 'PRINT#') or (Command = 'CMD') or (Command = 'APPEND') or (Command = 'WRITE#') then
  begin
    if Length(Data) > 0 then
      try FS.Write(Data[1], Length(Data)); except ErrorCode := 25; end;
  end
  else if Command = 'PUTBIN' then
  begin
    // Binary PUT: Data already holds the raw bytes to write (serialised by the VM).
    if Length(Data) > 0 then
      try FS.Write(Data[1], Length(Data)); except ErrorCode := 25; end;
  end
  else if Command = 'GETBIN' then
  begin
    // Binary GET: Data on input is the byte count to read; return the raw bytes read (fewer at EOF).
    i := StrToIntDef(Data, 0);
    if i < 0 then i := 0;
    SetLength(Data, i);
    if i > 0 then
    begin
      i := FS.Read(Data[1], i);
      SetLength(Data, i);
    end;
  end
  else if Command = 'RECORD' then
  begin
    try FS.Position := StrToInt64(Data); except
      on E: EConvertError do ErrorCode := 63;
      on E: Exception do ErrorCode := 70;
    end;
  end;
end;

end.
