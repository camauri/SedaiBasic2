unit SedaiFbString;

{$mode objfpc}{$H+}
{$codepage UTF8}

{ ⭐ PHASE 5 OF THE POINTER MODEL (6 Oct 2026): FreeBASIC's string DESCRIPTOR, the 24 bytes fbc's runtime calls
  FBSTRING - { data As ZString Ptr, len As Integer, size As Integer }.
  In the fb memory mode a String whose ADDRESS the program can see lives in one of these, so "@s" is the address of the
  descriptor, "StrPtr(s)" is its data pointer - the string's OWN bytes, which C can write - and a "String Ptr" reads and
  writes through it, exactly as under fbc. The bytes are libc's, as every program allocation is in that mode (phase 1).
  ⚠️ The descriptor is the ONLY truth for such a string: a read copies its bytes into a managed string, a write goes
  through FbStrSet. There is no second copy to keep in step.
  Measured on fbc 1.10 (scratchpad f5/m1.bas): the capacity grows only when the new length does not fit, to
  (len + len\8 + 32 + 3) and not 3 - 1 => 36, 36 => 72 -; it never shrinks; an empty assignment frees the bytes and zeroes
  all three fields; the bytes are NUL-terminated. }

interface

uses
  SysUtils;

type
  TFbStr = record
    Data: PAnsiChar;
    Len: PtrInt;
    Size: PtrInt;
  end;
  PFbStr = ^TFbStr;

const
  FBSTR_BYTES = SizeOf(TFbStr);   // 24, SizeOf(String) under fbc on a 64-bit target

function FbStrGet(D: PFbStr): string;
procedure FbStrSet(D: PFbStr; const S: string);
procedure FbStrClear(D: PFbStr);
// A deep copy into a ZEROED destination descriptor (an array or a record copied by value).
procedure FbStrDupInto(Dst, Src: PFbStr);

implementation

{$IFDEF WINDOWS}
function fbs_malloc(size: PtrUInt): Pointer; cdecl; external 'msvcrt' name 'malloc';
function fbs_realloc(p: Pointer; size: PtrUInt): Pointer; cdecl; external 'msvcrt' name 'realloc';
procedure fbs_free(p: Pointer); cdecl; external 'msvcrt' name 'free';
{$ELSE}
function fbs_malloc(size: PtrUInt): Pointer; cdecl; external 'c' name 'malloc';
function fbs_realloc(p: Pointer; size: PtrUInt): Pointer; cdecl; external 'c' name 'realloc';
procedure fbs_free(p: Pointer); cdecl; external 'c' name 'free';
{$ENDIF}

// fbc keeps a "temporary" flag in the top bit of len; a descriptor C or the program hands us never carries it, but a
// length read here never lets it through either.
const
  FB_TEMPSTRBIT = PtrInt(1) shl (SizeOf(PtrInt) * 8 - 1);

// FBSTR_DIAG=1: how many descriptor buffers were allocated and freed, printed at exit - the leak and double-free census of
// phase 5 (a record's implicit destructor, a deep copy). Plain counters: the cost is an increment.
var
  GAllocs, GFrees: Int64;

function FbStrGet(D: PFbStr): string;
var
  L: PtrInt;
begin
  Result := '';
  if D = nil then Exit;
  L := D^.Len and not FB_TEMPSTRBIT;
  if (L <= 0) or (D^.Data = nil) then Exit;
  SetString(Result, D^.Data, L);
end;

procedure FbStrClear(D: PFbStr);
begin
  if D = nil then Exit;
  if D^.Data <> nil then begin fbs_free(D^.Data); Inc(GFrees); end;
  D^.Data := nil;
  D^.Len := 0;
  D^.Size := 0;
end;

procedure FbStrSet(D: PFbStr; const S: string);
var
  L, NewSize: PtrInt;
  P: PAnsiChar;
begin
  if D = nil then Exit;
  L := Length(S);
  if L = 0 then
  begin
    FbStrClear(D);
    Exit;
  end;
  // ⚠️ S may BE this descriptor's own bytes (a read of it just before): the realloc below would then move them away
  // under the source. A managed string from FbStrGet is always a copy, so that cannot happen through this unit.
  if (D^.Data = nil) or (L > D^.Size) then
  begin
    NewSize := (L + (L shr 3) + 32 + 3) and not PtrInt(3);
    if D^.Data = nil then Inc(GAllocs);
    P := fbs_realloc(D^.Data, PtrUInt(NewSize) + 1);
    if P = nil then raise EOutOfMemory.Create('Out of memory (string)');
    D^.Data := P;
    D^.Size := NewSize;
  end;
  Move(S[1], D^.Data^, L);
  D^.Data[L] := #0;
  D^.Len := L;
end;

procedure FbStrDupInto(Dst, Src: PFbStr);
var
  L: PtrInt;
begin
  Dst^.Data := nil;
  Dst^.Len := 0;
  Dst^.Size := 0;
  if (Src = nil) or (Src^.Data = nil) then Exit;
  L := Src^.Len and not FB_TEMPSTRBIT;
  if L <= 0 then Exit;
  if Src^.Size < L then Dst^.Size := L else Dst^.Size := Src^.Size;
  Dst^.Data := fbs_malloc(PtrUInt(Dst^.Size) + 1);
  Inc(GAllocs);
  if Dst^.Data = nil then raise EOutOfMemory.Create('Out of memory (string)');
  Move(Src^.Data^, Dst^.Data^, L);
  Dst^.Data[L] := #0;
  Dst^.Len := L;
end;

var
  GFbStrDiag: Boolean = False;

procedure FbStrDiagReport;
begin
  if not GFbStrDiag then Exit;
  GFbStrDiag := False;
  WriteLn(ErrOutput, '[FBSTR] buffers allocated ', GAllocs, ', freed ', GFrees, ', live at exit ', GAllocs - GFrees);
end;

initialization
  // Printed through AddExitProc, as the VM's own censuses are: a program may end by a halt that skips finalization.
  GFbStrDiag := GetEnvironmentVariable('FBSTR_DIAG') = '1';
  if GFbStrDiag then AddExitProc(@FbStrDiagReport);
end.
