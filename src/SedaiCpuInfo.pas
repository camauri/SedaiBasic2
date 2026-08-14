unit SedaiCpuInfo;

{ How many processors this machine has - the two counts, and they are not the same number.

  LOGICAL processors are what the OS schedules threads onto: cores times threads-per-core.
  PHYSICAL cores are the real execution units. On this development machine (Core Ultra 9 185H)
  they are 22 and 16 - a program that sizes a worker pool needs to say WHICH it means.

  ⛔ Do NOT use System.CPUCount or TThread.ProcessorCount for this. Measured 12 Aug 2026 on
  FPC 3.2.2 / Linux: both return 1, with or without cthreads, while sysconf says 22. A worker
  count taken from them collapses the pool to a single thread IN SILENCE - the failure mode this
  unit exists to prevent.

  Both counts are asked of the OS once and cached: the answer cannot change while we run, and a
  worker loop must not pay a syscall per iteration.
}

{$mode objfpc}{$H+}

interface

{ Logical processors (hardware threads). Never less than 1. }
function LogicalProcessorCount: Integer;

{ Physical cores. Never less than 1, never more than the logical count. Falls back to the logical
  count where the OS will not say - reporting too many is the safer error for a worker pool than
  reporting one. }
function PhysicalCoreCount: Integer;

{ Physical CPUs: sockets, or packages. Never less than 1, never more than the core count. }
function PhysicalCpuCount: Integer;

implementation

uses
  SysUtils, Classes
  {$IFDEF WINDOWS}, Windows{$ENDIF};

var
  FLogical: Integer = 0;      // 0 = not asked yet
  FPhysical: Integer = 0;
  FSockets: Integer = 0;

{$IFDEF UNIX}
{ _SC_NPROCESSORS_ONLN is not a portable NUMBER - only a portable name - so it is spelled per OS.
  Linux/glibc: 84 (verified against nproc). Darwin: 58. }
const
  {$IFDEF DARWIN}
  SC_NPROCESSORS_ONLN = 58;
  {$ELSE}
  SC_NPROCESSORS_ONLN = 84;
  {$ENDIF}

function sysconf(name: LongInt): LongInt; cdecl; external 'c' name 'sysconf';
{$ENDIF}

{$IFDEF WINDOWS}
type
  TLogicalProcessorRelationship = (RelationProcessorCore, RelationNumaNode, RelationCache,
                                   RelationProcessorPackage, RelationGroup);
  TSystemLogicalProcessorInformation = record
    ProcessorMask: PtrUInt;
    Relationship: TLogicalProcessorRelationship;
    Reserved: array[0..1] of QWord;
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

function GetLogicalProcessorInformation(Buffer: PSystemLogicalProcessorInformation;
  var ReturnedLength: DWORD): BOOL; stdcall; external 'kernel32' name 'GetLogicalProcessorInformation';
{$ENDIF}

function QueryLogical: Integer;
{$IFDEF WINDOWS}
var
  si: SYSTEM_INFO;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF UNIX}
  Result := sysconf(SC_NPROCESSORS_ONLN);
  {$ENDIF}
  {$IFDEF WINDOWS}
  FillChar(si, SizeOf(si), 0);
  GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
  {$ENDIF}
  if Result < 1 then Result := 1;
end;

{$IFDEF UNIX}
{ Physical cores from /proc/cpuinfo: one per DISTINCT (physical id, core id) pair. Counting "core
  id" alone would merge the cores of two sockets that both number theirs from zero. }
function QueryPhysicalProc: Integer;
var
  F: TextFile;
  Line, Key, Val, PhysId: string;
  P: Integer;
  Seen: TStringList;
begin
  Result := 0;
  if not FileExists('/proc/cpuinfo') then Exit;
  Seen := TStringList.Create;
  try
    Seen.Sorted := True;
    Seen.Duplicates := dupIgnore;
    AssignFile(F, '/proc/cpuinfo');
    {$I-} Reset(F); {$I+}
    if IOResult <> 0 then Exit;
    try
      PhysId := '0';
      while not EOF(F) do
      begin
        ReadLn(F, Line);
        P := Pos(':', Line);
        if P = 0 then Continue;
        Key := LowerCase(Trim(Copy(Line, 1, P - 1)));
        Val := Trim(Copy(Line, P + 1, Length(Line)));
        if Key = 'physical id' then
          PhysId := Val
        else if Key = 'core id' then
          Seen.Add(PhysId + '/' + Val);
      end;
    finally
      CloseFile(F);
    end;
    Result := Seen.Count;
  finally
    Seen.Free;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
{ One walk of the table, counting entries of the asked-for relationship: cores are
  RelationProcessorCore, sockets are RelationProcessorPackage. }
function CountRelation(Rel: TLogicalProcessorRelationship): Integer;
var
  Len: DWORD;
  Buf: array of Byte;
  P: PSystemLogicalProcessorInformation;
  Offset, Step: PtrUInt;
begin
  Result := 0;
  Len := 0;
  { The first call is expected to fail: it is how the required size is asked for. }
  GetLogicalProcessorInformation(nil, Len);
  if Len = 0 then Exit;
  SetLength(Buf, Len);
  if not GetLogicalProcessorInformation(PSystemLogicalProcessorInformation(@Buf[0]), Len) then Exit;
  Step := SizeOf(TSystemLogicalProcessorInformation);
  Offset := 0;
  while Offset + Step <= Len do
  begin
    P := PSystemLogicalProcessorInformation(@Buf[Offset]);
    if P^.Relationship = Rel then Inc(Result);
    Inc(Offset, Step);
  end;
end;

function QueryPhysicalWin: Integer;
begin
  Result := CountRelation(RelationProcessorCore);
end;
{$ENDIF}

function QueryPhysical: Integer;
begin
  Result := 0;
  {$IFDEF UNIX}
  Result := QueryPhysicalProc;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := QueryPhysicalWin;
  {$ENDIF}
  { Unknown, or a nonsense answer: say the logical count rather than invent one. }
  if (Result < 1) or (Result > LogicalProcessorCount) then
    Result := LogicalProcessorCount;
end;

{ Sockets. On Linux the distinct "physical id" values; where the file does not carry them (a single
  package, a VM, an ARM board) the honest answer is 1. }
function QuerySockets: Integer;
{$IFDEF UNIX}
var
  F: TextFile;
  Line, Key, Val: string;
  P: Integer;
  Seen: TStringList;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF UNIX}
  if FileExists('/proc/cpuinfo') then
  begin
    Seen := TStringList.Create;
    try
      Seen.Sorted := True;
      Seen.Duplicates := dupIgnore;
      AssignFile(F, '/proc/cpuinfo');
      {$I-} Reset(F); {$I+}
      if IOResult = 0 then
      begin
        try
          while not EOF(F) do
          begin
            ReadLn(F, Line);
            P := Pos(':', Line);
            if P = 0 then Continue;
            Key := LowerCase(Trim(Copy(Line, 1, P - 1)));
            Val := Trim(Copy(Line, P + 1, Length(Line)));
            if Key = 'physical id' then Seen.Add(Val);
          end;
        finally
          CloseFile(F);
        end;
        Result := Seen.Count;
      end;
    finally
      Seen.Free;
    end;
  end;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := CountRelation(RelationProcessorPackage);
  {$ENDIF}
  if (Result < 1) or (Result > PhysicalCoreCount) then Result := 1;
end;

function LogicalProcessorCount: Integer;
begin
  if FLogical = 0 then FLogical := QueryLogical;
  Result := FLogical;
end;

function PhysicalCpuCount: Integer;
begin
  if FSockets = 0 then FSockets := QuerySockets;
  Result := FSockets;
end;

function PhysicalCoreCount: Integer;
begin
  if FPhysical = 0 then FPhysical := QueryPhysical;
  Result := FPhysical;
end;

end.
