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
  'R' read, 'W' write/truncate, 'A' append, 'B' binary (read+write, no truncate).
  A trailing '<' is FreeBASIC's "ACCESS READ": read-only, and - the part that shows -
  a MISSING file is an error instead of being created. It is a separate marker rather
  than an 'R' so that FILEATTR keeps reporting the mode letter fbc reports. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiBytecodeVM
  // Only to ask whether stdin is a console: the buffered reader must not read ahead on a terminal.
  {$IFDEF WINDOWS}, Windows{$ELSE}, termio{$ENDIF};

type
  TVMFileHandler = class
  private
    FFileHandles: array[1..15] of TFileStream;
    // FreeBASIC standard DEVICES opened as ordinary handles: "Open Cons For Input As #1" is how a
    // program reads stdin, and CLBG's reverse-complement / k-nucleotide / regex-redux are all written
    // that way. A device has no TFileStream - the bytes come from System.Input and go to System.Output -
    // so it needs its own marker, and the handle counts as open while FFileHandles stays nil.
    //   1 = CONS (stdin when opened For Input, stdout when For Output)   2 = SCRN (stdout)   3 = ERR (stderr)
    FDeviceKind: array[1..15] of Integer;
    FFileModes: array[1..15] of string;
    FRecordLens: array[1..15] of Integer;   // relative-file record length per handle (0 = not relative)

    { ===== Cached file size =====

      TFileStream.Size is not a field, it is three seeks: remember where we are, seek to the end, seek
      back. Measured on this machine that is 4.4 us, and Position is another 1.2 - so `FS.Position >=
      FS.Size`, which is all EOF is, cost 5.6 us PER CALL. Every "Do While Not Eof(fh)" pays it once a
      line, and the old INPUT#/LINEINPUT# loop paid it once a BYTE.

      The size of an open file only changes when WE write to it, so it is cached per handle and
      invalidated in the one place that writes. -1 = not known yet. ⚠️ This deliberately does not
      model another process appending to a file we hold open; neither does FreeBASIC, and paying four
      syscalls per character to find out is not a trade anyone asked for. }
    FSizeCache: array[1..15] of Int64;

    { ===== Buffered standard input =====

      "Line Input #1" on a device handle used to go straight to System.ReadLn(System.Input), and FPC's
      Text layer costs microseconds PER CALL - the same thing that made writing slow until stdout was
      buffered. Measured on reverse-complement, 416 671 lines of 60 characters: 1.42 us per line, 590 ms
      of a 1977 ms program, the single largest item in it.

      So the bytes are pulled in blocks with FileRead and the lines are cut here. EOF is answered from
      the buffer too, which removes a second Text-layer call per line.

      ⛔ ONLY when stdin is REDIRECTED. On an interactive console the program and the user take turns,
      and reading ahead would swallow input that a later prompt is supposed to see; there the old
      per-line path stays. FInBufMode: 0 = not decided yet, 1 = buffered, 2 = pass through. }
    FInBuf: array[0 .. 65535] of Byte;
    FInLen: Integer;        // bytes currently in FInBuf
    FInPos: Integer;        // next byte to consume
    FInEof: Boolean;        // the OS said "no more"
    FInBufMode: Integer;
    function CachedSize(Handle: Integer; FS: TFileStream): Int64;
    procedure InvalidateSize(Handle: Integer);
    function RecordUnit(Handle: Integer): Int64;
    function StdInBuffered: Boolean;
    function StdInRefill: Boolean;              // returns False at end of input
    function StdInReadLine(out Line: string): Boolean;
    function StdInAtEof: Boolean;
  public
    destructor Destroy; override;
    procedure CloseAll;
    // Wire these to VM.OnDiskFile / VM.OnFileData.
    procedure DiskFile(Sender: TBytecodeVM; const Command: string; Handle: Integer;
      const HandleName, Filename, Mode: string; var ErrorCode: Integer);
    procedure FileData(Sender: TBytecodeVM; const Command: string; Handle: Integer;
      var Data: string; var ErrorCode: Integer);
    // Wire to VM.OnFileQuery. The numeric answer to EOF/FREEFILE/LOF/LOC/SEEK, and the SINGLE place
    // those five are computed: the string arms of FileData delegate here rather than repeating the
    // rules, so the two protocols cannot drift apart.
    function FileQuery(Sender: TBytecodeVM; QueryCode, Handle: Integer;
      out Value: Int64; out ErrorCode: Integer): Boolean;
  end;

implementation

{ ===== Buffered standard input - see the fields' comment for why =====

  ⚠️ These use the RAW stdin handle, so they must never be mixed with System.ReadLn(System.Input) on
  the same run: the Text layer keeps its own buffer and the two would each swallow part of the stream.
  StdInBuffered decides ONCE, and every device read goes through one path or the other for good. }

function TVMFileHandler.CachedSize(Handle: Integer; FS: TFileStream): Int64;
begin
  if (Handle < 1) or (Handle > 15) then Exit(FS.Size);
  if FSizeCache[Handle] < 0 then FSizeCache[Handle] := FS.Size;
  Result := FSizeCache[Handle];
end;

procedure TVMFileHandler.InvalidateSize(Handle: Integer);
begin
  if (Handle >= 1) and (Handle <= 15) then FSizeCache[Handle] := -1;
end;

// LOC counts in RECORDS, and what a record IS depends only on how the file was opened
// (job/fb-manual/KeyPgLoc.html): the length given at Open for RANDOM, ONE byte for BINARY, and a
// record length of 128 bytes ASSUMED for a text file - the classic BASIC convention. Confirmed
// against fbc 1.10.1 on this machine rather than read off the manual: text position 4097 answers 32,
// binary answers the byte count, and RANDOM Len=32 answers the record number.
// Mode letters are the parser's own single letters ('R' 'W' 'A' 'B', 'L<n>' for random, plus a
// trailing '<' for ACCESS READ), so testing for 'B' cannot collide with a filename.
function TVMFileHandler.RecordUnit(Handle: Integer): Int64;
begin
  if (Handle < 1) or (Handle > 15) then Exit(1);
  if FRecordLens[Handle] > 0 then Result := FRecordLens[Handle]
  else if Pos('B', FFileModes[Handle]) > 0 then Result := 1
  else Result := 128;
end;

function TVMFileHandler.StdInBuffered: Boolean;
{$IFDEF WINDOWS}
const FILE_TYPE_CHAR = $0002;
{$ENDIF}
begin
  if FInBufMode = 0 then
  begin
    FInBufMode := 2;                       // pass through unless proven redirected
    {$IFDEF WINDOWS}
    // A character device is the console: a program reading it is talking to a person, and reading
    // ahead would take input meant for a later prompt.
    if GetFileType(StdInputHandle) <> FILE_TYPE_CHAR then FInBufMode := 1;
    {$ELSE}
    if IsATTY(StdInputHandle) = 0 then FInBufMode := 1;
    {$ENDIF}
    if FInBufMode = 1 then begin FInLen := 0; FInPos := 0; FInEof := False; end;
  end;
  Result := FInBufMode = 1;
end;

function TVMFileHandler.StdInRefill: Boolean;
// Pull the next block. Returns False only when the input is genuinely finished.
var
  N: LongInt;
begin
  if FInPos < FInLen then Exit(True);
  if FInEof then Exit(False);
  N := FileRead(StdInputHandle, FInBuf[0], SizeOf(FInBuf));
  if N <= 0 then begin FInEof := True; FInLen := 0; FInPos := 0; Exit(False); end;
  FInLen := N; FInPos := 0;
  Result := True;
end;

function TVMFileHandler.StdInAtEof: Boolean;
// EOF is answered from the buffer, which is the second Text-layer call per line that disappears.
begin
  Result := (not StdInRefill);
end;

function TVMFileHandler.StdInReadLine(out Line: string): Boolean;
// One line, without its terminator. Handles LF and CRLF, and a last line with no terminator at all.
// The line is assembled with Move out of the block, so a 60-character line costs no per-character work.
var
  Start, i, Chunk, OldLen: Integer;
  Found: Boolean;
begin
  Line := '';
  if not StdInRefill then Exit(False);
  Found := False;
  repeat
    Start := FInPos;
    i := FInPos;
    while (i < FInLen) and (FInBuf[i] <> 10) do Inc(i);
    Chunk := i - Start;
    if Chunk > 0 then
    begin
      OldLen := Length(Line);
      SetLength(Line, OldLen + Chunk);
      Move(FInBuf[Start], Line[OldLen + 1], Chunk);
    end;
    if i < FInLen then
    begin
      FInPos := i + 1;                     // step over the LF
      Found := True;
    end
    else
    begin
      FInPos := FInLen;                    // block exhausted: the line continues in the next one
      if not StdInRefill then Found := True;   // no more input: what we have IS the last line
    end;
  until Found;
  // A CRLF stream leaves the CR at the end of the line; BASIC must not see it.
  if (Length(Line) > 0) and (Line[Length(Line)] = #13) then SetLength(Line, Length(Line) - 1);
  Result := True;
end;

{$IFDEF WINDOWS}
// Windows resolves a handful of legacy DOS names to DEVICES rather than files: opening 'LPT1:' or 'PRN'
// for output hands the bytes to the PRINT SPOOLER, and 'COM1' to the serial port. The filename here comes
// straight out of the BASIC program, and the sweeps run programs downloaded from the web unattended — so
// this VM refuses devices and does storage only. The match has to be as loose as the OS's own: a trailing
// colon, an extension ('LPT1.TXT' is still the printer) and trailing blanks are all ignored by Windows.
function IsReservedDeviceName(const Filename: string): Boolean;
var
  S: string;
  i, P: Integer;
begin
  S := UpperCase(Trim(Filename));
  // Last path component, without ExtractFileName: that treats ':' as a drive separator and would swallow
  // the whole of 'LPT1:', leaving nothing to test.
  for i := Length(S) downto 1 do
    if (S[i] = '\') or (S[i] = '/') then
    begin
      S := Copy(S, i + 1, Length(S) - i);
      Break;
    end;
  P := Pos(':', S); if P > 0 then S := Copy(S, 1, P - 1);
  P := Pos('.', S); if P > 0 then S := Copy(S, 1, P - 1);
  S := TrimRight(S);
  Result := (S = 'CON') or (S = 'PRN') or (S = 'AUX') or (S = 'NUL') or
            (((Copy(S, 1, 3) = 'COM') or (Copy(S, 1, 3) = 'LPT')) and (Length(S) = 4) and
             (S[4] >= '1') and (S[4] <= '9'));
end;
{$ENDIF}

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
      FRecordLens[i] := 0;
    end;
end;

procedure TVMFileHandler.DiskFile(Sender: TBytecodeVM; const Command: string; Handle: Integer;
  const HandleName, Filename, Mode: string; var ErrorCode: Integer);
var
  M: string;
  FileMode: Word;
begin
  ErrorCode := 0;
  // DCLEAR / RESET: close every open handle. Signalled with Handle 0, so it must be handled before
  // the per-handle range check below (which would otherwise reject Handle 0 with error 64).
  if Command = 'DCLEAR' then begin CloseAll; Exit; end;
  if (Handle < 1) or (Handle > 15) then begin ErrorCode := 64; Exit; end;

  if Command = 'DOPEN' then
  begin
    // A FreeBASIC standard device, marked as such by the parser. No file is opened: the handle is bound
    // to the process's own streams. This must come BEFORE the reserved-name refusal below, which exists
    // to stop a PROGRAM from reaching the printer or a serial port by naming a DOS device in a string -
    // a different thing entirely from the language's own CONS/SCRN/ERR.
    if (Filename = 'CONS:') or (Filename = 'SCRN:') or (Filename = 'ERR:') then
    begin
      if Assigned(FFileHandles[Handle]) then
      begin
        FreeAndNil(FFileHandles[Handle]);
        FFileModes[Handle] := '';
      end;
      if Filename = 'CONS:' then FDeviceKind[Handle] := 1
      else if Filename = 'SCRN:' then FDeviceKind[Handle] := 2
      else FDeviceKind[Handle] := 3;
      FFileModes[Handle] := UpperCase(Mode);
      FRecordLens[Handle] := 0;
      Exit;
    end;
    {$IFDEF WINDOWS}
    // A device is not a file: refuse it as one (62 = FILE NOT FOUND). See IsReservedDeviceName.
    if IsReservedDeviceName(Filename) then begin ErrorCode := 62; Exit; end;
    {$ENDIF}
    if Assigned(FFileHandles[Handle]) then
    begin
      FreeAndNil(FFileHandles[Handle]);
      FFileModes[Handle] := '';
    end;
    FRecordLens[Handle] := 0;
    M := UpperCase(Mode);
    // Relative file "L<reclen>": random-access, read+write, created if absent (never truncated).
    if (Length(M) >= 1) and (M[1] = 'L') then
    begin
      if FileExists(Filename) then FileMode := fmOpenReadWrite else FileMode := fmCreate;
      try
        FFileHandles[Handle] := TFileStream.Create(Filename, FileMode);
        InvalidateSize(Handle);
        FFileModes[Handle] := M;
        FRecordLens[Handle] := StrToIntDef(Copy(M, 2, Length(M) - 1), 1);
        if FRecordLens[Handle] < 1 then FRecordLens[Handle] := 1;
      except
        on E: EFCreateError do begin ErrorCode := 26; FFileHandles[Handle] := nil; end;
        on E: Exception do begin ErrorCode := 70; FFileHandles[Handle] := nil; end;
      end;
      Exit;
    end;
    // "ACCESS READ" (trailing '<') never creates: "Open f For Binary Access Read As #h" on a missing
    // file is an error in fbc, where a plain "For Binary" creates the file. Checked BEFORE the mode
    // letters, since 'B' alone would otherwise create it.
    if not FileExists(Filename) and
       ((Pos('<', M) > 0) or
        ((Pos('W', M) = 0) and (Pos('A', M) = 0) and (Pos('B', M) = 0))) then
    begin
      ErrorCode := 62;  // FILE NOT FOUND (read of a missing file)
      Exit;
    end;
    if Pos('<', M) > 0 then
      FileMode := fmOpenRead or fmShareDenyNone
    else if Pos('W', M) > 0 then
      FileMode := fmCreate
    else if (Pos('A', M) > 0) or (Pos('B', M) > 0) then
    begin
      if FileExists(Filename) then FileMode := fmOpenReadWrite else FileMode := fmCreate;
    end
    else
      FileMode := fmOpenRead or fmShareDenyNone;
    try
      FFileHandles[Handle] := TFileStream.Create(Filename, FileMode);
      InvalidateSize(Handle);
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
    FDeviceKind[Handle] := 0;      // ...and release the handle if it was a device
    FRecordLens[Handle] := 0;
  end;
end;

function TVMFileHandler.FileQuery(Sender: TBytecodeVM; QueryCode, Handle: Integer;
  out Value: Int64; out ErrorCode: Integer): Boolean;
// Every rule for the five queries lives HERE. FileData's string arms call into this function, so
// there is one implementation and one place to change - a gate built by duplicating a body diverges
// and then lies about it.
var
  i: Integer;
  FS: TFileStream;
begin
  Result := True;
  Value := 0;
  ErrorCode := 0;

  // FREEFILE: lowest unused handle 1..15 (0 if none). Does not need an open handle.
  if QueryCode = FQ_FREEFILE then
  begin
    for i := 1 to 15 do
      if not Assigned(FFileHandles[i]) then begin Value := i; Exit; end;
    Exit;                                   // none free -> 0
  end;

  // A standard DEVICE handle (CONS/SCRN/ERR) has no stream behind it.
  if (Handle >= 1) and (Handle <= 15) and (FDeviceKind[Handle] <> 0) then
  begin
    if QueryCode = FQ_EOF then
    begin
      if FDeviceKind[Handle] <> 1 then Exit;            // output device: never at EOF
      if StdInBuffered then
      begin
        if StdInAtEof then Value := -1;
      end
      else if System.Eof(System.Input) then Value := -1;
    end;
    Exit;                                   // LOF/LOC/SEEK mean nothing on a device -> 0
  end;

  if (Handle < 1) or (Handle > 15) or (not Assigned(FFileHandles[Handle])) then
  begin
    ErrorCode := 64;                        // FILE NOT OPEN
    if QueryCode = FQ_EOF then Value := -1; // EOF of a closed file = true
    Exit;
  end;

  FS := FFileHandles[Handle];
  case QueryCode of
    FQ_EOF:  Value := -Ord(FS.Position >= CachedSize(Handle, FS));  // FB: -1 (true) at/after end of file
    FQ_LOF:  Value := CachedSize(Handle, FS);
    // LOC is a RECORD number, never a byte offset - see RecordUnit for what a record is in each mode.
    // It is NOT "the last read/write" in any stateful sense despite the wording in the manual: fbc
    // answers straight from the file position, so a bare SEEK with no I/O after it still moves LOC.
    FQ_LOC:  Value := FS.Position div RecordUnit(Handle);
    // SEEK is 1-based and counts records ONLY for RANDOM; bytes in every other mode
    // (job/fb-manual/KeyPgSeekreturn.html, and fbc agrees - text and binary both answer Position+1).
    // ⚠️ ONE fbc DIFFERENCE HERE IS DELIBERATE. On a text file terminated with bare LF, fbc's SEEK
    // reports a number that is NOT the position - 10 after consuming 11 bytes - with the error
    // shrinking to zero at EOF. It fails its own round trip: feed it back to the SEEK statement and
    // fbc lands mid-line, reading "9" where it had just read "abcdefghij" (the measurement is
    // job/tests/bench/seek_roundtrip.bas). That is an artefact of its buffered reader, not a
    // semantic, and reproducing it would make SEEK unusable as a position. With CRLF, BINARY and
    // RANDOM fbc is exact and so are we - locked down by job/tests/bas/bug_loc_seek_records.bas.
    FQ_SEEK: if FRecordLens[Handle] > 0 then
               Value := FS.Position div FRecordLens[Handle] + 1
             else
               Value := FS.Position + 1;
  else
    Result := False;                        // not a query we know: let the caller use the strings
  end;
end;

procedure TVMFileHandler.FileData(Sender: TBytecodeVM; const Command: string; Handle: Integer;
  var Data: string; var ErrorCode: Integer);
var
  Ch: Byte;
  Line: string;
  i: Integer;
  FS: TFileStream;
  M: string;
  RetType, V: Integer;
  QV: Int64;           // the numeric answer from FileQuery, before it is turned into a string
  LBuf: array[0..4095] of Byte;   // line reader, see INPUT#/LINEINPUT#
  LStart, LSize: Int64;
  LGot, LIdx, LOld, LUsed: Integer;
  LTerm: Boolean;
begin
  ErrorCode := 0;

  // The five QUERIES are computed by FileQuery, which is the single source of their rules; this arm
  // only turns the number into the string this protocol carries.
  if Command = 'FREEFILE' then
  begin
    FileQuery(Sender, FQ_FREEFILE, Handle, QV, ErrorCode);
    Data := IntToStr(QV);
    Exit;
  end;

  // FILEATTR(filenum, returntype): info about an open file number (returntype passed in via Data,
  // result written back to Data). 1 = File Mode (sum of Input1/Output2/Random4/Append8/Binary32),
  // 2 = OS file handle, 3 = Encoding (0 = ASCII, byte stream). An invalid/closed handle yields 0.
  // Handled before the range check so a bad handle returns 0 rather than a fatal "file not open".
  if Command = 'FILEATTR' then
  begin
    RetType := StrToIntDef(Data, 1);
    Data := '0';
    if (Handle >= 1) and (Handle <= 15) and Assigned(FFileHandles[Handle]) then
      case RetType of
        2: Data := IntToStr(PtrInt(FFileHandles[Handle].Handle));  // OS file handle
        3: Data := '0';   // Encoding: ASCII
      else
        begin
          M := UpperCase(FFileModes[Handle]);
          V := 0;
          if (Length(M) >= 1) and (M[1] = 'L') then
            V := 4                                    // relative file = Random
          else
          begin
            if Pos('R', M) > 0 then V := V or 1;      // Input
            if Pos('W', M) > 0 then V := V or 2;      // Output
            if Pos('A', M) > 0 then V := V or 8;      // Append
            if Pos('B', M) > 0 then V := V or 32;     // Binary
          end;
          Data := IntToStr(V);
        end;
      end;
    Exit;
  end;

  // A standard DEVICE handle (CONS/SCRN/ERR) has no stream behind it: serve it from the process's own
  // input and output before the "is there a TFileStream?" test below, which would call it "not open".
  if (Handle >= 1) and (Handle <= 15) and (FDeviceKind[Handle] <> 0) then
  begin
    if Command = 'EOF' then
    begin
      FileQuery(Sender, FQ_EOF, Handle, QV, ErrorCode);
      Data := IntToStr(QV);
      Exit;
    end;
    if (Command = 'INPUT#') or (Command = 'LINEINPUT#') then
    begin
      if StdInBuffered then
      begin
        if not StdInReadLine(Line) then begin Data := ''; ErrorCode := 62; Exit; end;
        Data := Line;
        Exit;
      end;
      if System.Eof(System.Input) then begin Data := ''; ErrorCode := 62; Exit; end;
      System.ReadLn(System.Input, Line);
      Data := Line;
      Exit;
    end;
    if (Command = 'PRINT#') or (Command = 'WRITE#') or (Command = 'CMD') or (Command = 'APPEND') then
    begin
      if FDeviceKind[Handle] = 3 then System.Write(System.ErrOutput, Data)
      else System.Write(System.Output, Data);
      Exit;
    end;
    if Command = 'DCLOSE' then begin FDeviceKind[Handle] := 0; FFileModes[Handle] := ''; Exit; end;
    Exit;   // LOF/LOC/SEEK and the record commands mean nothing on a device
  end;

  if (Handle < 1) or (Handle > 15) or (not Assigned(FFileHandles[Handle])) then
  begin
    ErrorCode := 64;  // FILE NOT OPEN
    if (Command = 'EOF') then Data := '-1';   // EOF of a closed file = true
    Exit;
  end;
  FS := FFileHandles[Handle];

  if Command = 'FILESETEOF' then
  begin
    // FreeBASIC FILESETEOF: set the file length to the current position (truncate if before EOF, extend
    // with zero bytes if beyond). After the call the position is at the (new) end. Status 0 = success.
    try
      FS.Size := FS.Position;
      FS.Position := CachedSize(Handle, FS);
      Data := '0';
    except
      ErrorCode := 63; Data := IntToStr(ErrorCode);   // could not set the file size
    end;
    Exit;
  end;

  if Command = 'EOF' then
    begin FileQuery(Sender, FQ_EOF, Handle, QV, ErrorCode); Data := IntToStr(QV); end
  else if Command = 'LOF' then
    begin FileQuery(Sender, FQ_LOF, Handle, QV, ErrorCode); Data := IntToStr(QV); end
  else if Command = 'LOC' then
    begin FileQuery(Sender, FQ_LOC, Handle, QV, ErrorCode); Data := IntToStr(QV); end
  else if Command = 'SEEK' then
    begin FileQuery(Sender, FQ_SEEK, Handle, QV, ErrorCode); Data := IntToStr(QV); end
  else if Command = 'SEEKSET' then
  begin
    // SEEK #n, pos statement: set the 1-based position. "The position is given in RECORDS if the file
    // was opened in Random access mode, in bytes in any other case" (FB manual, KeyPgSeekset) -- which
    // is also how "Put #n, recno, rec" and "Get #n, recno, rec" address a random-access file, since both
    // position through this command.
    try
      if FRecordLens[Handle] > 0 then
        FS.Position := (StrToInt64(Data) - 1) * FRecordLens[Handle]
      else
        FS.Position := StrToInt64(Data) - 1;
    except ErrorCode := 63; end;
  end
  else if Command = 'GET#' then
  begin
    if FS.Read(Ch, 1) > 0 then Data := Chr(Ch) else Data := '';
  end
  else if (Command = 'INPUT#') or (Command = 'LINEINPUT#') then
  begin
    { Read a BLOCK, cut the line out of it, then put the stream back exactly where reading one byte
      at a time would have left it.

      The old loop cost, per CHARACTER: `FS.Position < FS.Size` (four seeks, measured 5.6 us), a
      one-byte FS.Read, and `Line := Line + Chr(Ch)` (a reallocation). Measured on a 2000-line file of
      79 columns that was 712 us PER LINE - 9 us per character, 1.4 seconds to read 158 KB.

      ⛔ Deliberately NOT a persistent per-handle buffer. That would make the stream position stop
      meaning the logical position, and SEEK/LOC/GET/PUT/RECORD all read it - the same aliasing trap
      the buffered-stdin comment above warns about. Reading ahead and seeking back keeps every other
      command looking at exactly what it looked at before, at the cost of one extra seek per line. }
    LStart := FS.Position;
    LSize := CachedSize(Handle, FS);
    if LStart >= LSize then begin ErrorCode := 62; Data := ''; Exit; end;
    Line := '';
    LUsed := 0;                          // bytes of the file consumed, terminator included
    LTerm := False;
    while not LTerm do
    begin
      FS.Position := LStart + LUsed;
      LGot := FS.Read(LBuf, SizeOf(LBuf));
      if LGot <= 0 then Break;           // the file ended without a terminator
      LIdx := 0;
      while LIdx < LGot do
      begin
        if (LBuf[LIdx] = 10) or (LBuf[LIdx] = 13) then Break;
        // A comma separates fields for INPUT#, but is ordinary text for LINE INPUT#.
        if (LBuf[LIdx] = Ord(',')) and (Command = 'INPUT#') then Break;
        Inc(LIdx);
      end;
      if LIdx > 0 then                   // the data before the terminator, appended in one Move
      begin
        LOld := Length(Line);
        SetLength(Line, LOld + LIdx);
        Move(LBuf[0], Line[LOld + 1], LIdx);
      end;
      Inc(LUsed, LIdx);
      if LIdx >= LGot then
      begin
        // No terminator in this block. Stop only if that was the end of the file.
        if LStart + LUsed >= LSize then Break;
        Continue;
      end;
      Ch := LBuf[LIdx];
      Inc(LUsed);                        // the terminator is consumed, as the old loop consumed it
      if Ch = 13 then
      begin
        // CRLF counts as ONE terminator; a lone CR does not swallow the byte after it.
        if LIdx + 1 < LGot then
        begin
          if LBuf[LIdx + 1] = 10 then Inc(LUsed);
        end
        else if LStart + LUsed < LSize then
        begin
          // The LF, if there is one, fell just past this block.
          FS.Position := LStart + LUsed;
          if (FS.Read(Ch, 1) = 1) and (Ch = 10) then Inc(LUsed);
        end;
      end;
      LTerm := True;
    end;
    FS.Position := LStart + LUsed;       // where the byte-at-a-time loop would have stopped
    Data := Line;
  end
  else if (Command = 'PRINT#') or (Command = 'CMD') or (Command = 'APPEND') or (Command = 'WRITE#') then
  begin
    if Length(Data) > 0 then
      try FS.Write(Data[1], Length(Data)); InvalidateSize(Handle); except ErrorCode := 25; end;
  end
  else if Command = 'PUTBIN' then
  begin
    // Binary PUT: Data already holds the raw bytes to write (serialised by the VM).
    if Length(Data) > 0 then
      try FS.Write(Data[1], Length(Data)); InvalidateSize(Handle); except ErrorCode := 25; end;
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
    // Relative file: RECORD #n, recnum positions to record recnum (1-based) -> byte (recnum-1)*reclen.
    // A non-relative handle keeps the historic raw-byte behaviour.
    try
      if FRecordLens[Handle] > 0 then
        FS.Position := (StrToInt64(Data) - 1) * FRecordLens[Handle]
      else
        FS.Position := StrToInt64(Data);
    except
      on E: EConvertError do ErrorCode := 63;
      on E: Exception do ErrorCode := 70;
    end;
  end;
end;

end.
