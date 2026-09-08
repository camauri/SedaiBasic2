unit SedaiForeignDecl;

{$mode objfpc}{$H+}
{$codepage UTF8}

// ⭐ THE FORMAT OF A FOREIGN DECLARATION, and the only place that knows it.
//
// A "Declare Function f Alias "sym" Lib "z" (ByVal a As Integer) As Long" is recorded by the parser as
// one line of text:
//
//     NAME|SYMBOL|LIBRARY|RETURNTYPE|PARAMTYPE,PARAMTYPE,...
//
// and that text travels unchanged through the SSA, the bytecode program and the .basc file. It is text
// rather than a record because every reader wants a different slice of it - the SSA wants the BANK of
// each parameter, the VM wants its C TYPE - and a record would have to be serialized field by field for
// no gain at all.
//
// ⛔ THIS UNIT NAMES NO PROVIDER. It does not use SedaiFFI, does not know libffi exists, and answers
// only questions about the declaration itself. The core may depend on it; the marshalling lives in the
// provider (see src/SedaiFFI.pas and the note there on why the FFI is the BOUNDARY, not a fundamental
// function). DIVERGENZE 183.

interface

uses
  SysUtils;

type
  { What a declared BASIC type means to a C ABI. fkUnknown is NOT a fallback that gets guessed at: a
    caller that sees it must refuse, because a wrongly classified argument does not raise - it answers
    wrong numbers. }
  TForeignKind = (fkUnknown, fkVoid, fkS8, fkU8, fkS16, fkU16, fkS32, fkU32,
                  fkS64, fkU64, fkFloat, fkDouble, fkPointer);

  TForeignDecl = record
    Name: string;                    // the BASIC name, upper case
    Symbol: string;                  // the C symbol, in its own case
    LibName: string;                 // the library named on the declaration ('' = use #inclib)
    RetTypeName: string;             // '' for a SUB
    ParamTypeNames: array of string;
  end;

{ Split one table line. Returns False on a line that is not in the format above. }
function ParseForeignDecl(const ALine: string; out ADecl: TForeignDecl): Boolean;

{ Classify one declared type name. A trailing " PTR" (at any depth) makes it a pointer whatever it
  points at, which is why it is tested first. An unrecognised name answers fkUnknown. }
function ForeignKindOf(const ATypeName: string): TForeignKind;

{ How many bytes the kind occupies, for the buffer an argument is marshalled into. }
function ForeignKindSize(AKind: TForeignKind): Integer;

{ True when the kind travels in the FLOAT bank; everything else travels in the int bank. }
function ForeignKindIsFloat(AKind: TForeignKind): Boolean;

implementation

function ParseForeignDecl(const ALine: string; out ADecl: TForeignDecl): Boolean;
var
  Fields: array[0..4] of string;
  i, p, Start, n: Integer;
  Rest, T: string;
begin
  Result := False;
  ADecl.Name := ''; ADecl.Symbol := ''; ADecl.LibName := '';
  ADecl.RetTypeName := ''; SetLength(ADecl.ParamTypeNames, 0);
  if ALine = '' then Exit;
  Rest := ALine;
  for i := 0 to 3 do
  begin
    p := Pos('|', Rest);
    if p = 0 then Exit;                 // fewer than five fields: not one of ours
    Fields[i] := Copy(Rest, 1, p - 1);
    Rest := Copy(Rest, p + 1, MaxInt);
  end;
  Fields[4] := Rest;
  if Fields[0] = '' then Exit;
  ADecl.Name := Fields[0];
  ADecl.Symbol := Fields[1];
  ADecl.LibName := Fields[2];
  ADecl.RetTypeName := Trim(Fields[3]);
  // The parameter list: comma-separated, and EMPTY means no parameters (not one empty parameter).
  if Trim(Fields[4]) <> '' then
  begin
    n := 0;
    Start := 1;
    for i := 1 to Length(Fields[4]) + 1 do
      if (i > Length(Fields[4])) or (Fields[4][i] = ',') then
      begin
        T := Trim(Copy(Fields[4], Start, i - Start));
        if T <> '' then
        begin
          SetLength(ADecl.ParamTypeNames, n + 1);
          ADecl.ParamTypeNames[n] := T;
          Inc(n);
        end;
        Start := i + 1;
      end;
  end;
  Result := True;
end;

function ForeignKindOf(const ATypeName: string): TForeignKind;
var
  T: string;
begin
  T := UpperCase(Trim(ATypeName));
  if T = '' then Exit(fkVoid);
  // A POINTER is a pointer whatever it points at, and the suffix can repeat ("Any Ptr Ptr").
  if (Length(T) >= 4) and (Copy(T, Length(T) - 3, 4) = ' PTR') then Exit(fkPointer);
  if (T = 'ANY') then Exit(fkPointer);           // "As Any" only ever appears as a pointer here
  if (T = 'BYTE') then Exit(fkS8);
  if (T = 'UBYTE') or (T = 'BOOLEAN') then Exit(fkU8);
  if (T = 'SHORT') then Exit(fkS16);
  if (T = 'USHORT') then Exit(fkU16);
  if (T = 'LONG') then Exit(fkS32);
  if (T = 'ULONG') then Exit(fkU32);
  // ⛔ INTEGER IS POINTER-WIDE IN FreeBASIC, so on this 64-bit target it is 8 bytes, not 4. Getting
  // this wrong is the silent kind: the extra four bytes come from whatever was next in the buffer.
  if (T = 'INTEGER') or (T = 'LONGINT') then Exit(fkS64);
  if (T = 'UINTEGER') or (T = 'ULONGINT') then Exit(fkU64);
  if (T = 'SINGLE') then Exit(fkFloat);
  if (T = 'DOUBLE') then Exit(fkDouble);
  // ZSTRING / WSTRING with no PTR is a fixed buffer in a UDT, never a scalar parameter; a STRING
  // parameter of a foreign function is the address of its bytes.
  if (T = 'STRING') or (T = 'ZSTRING') or (T = 'WSTRING') then Exit(fkPointer);
  Result := fkUnknown;
end;

function ForeignKindSize(AKind: TForeignKind): Integer;
begin
  case AKind of
    fkVoid:                Result := 0;
    fkS8, fkU8:            Result := 1;
    fkS16, fkU16:          Result := 2;
    fkS32, fkU32, fkFloat: Result := 4;
  else
    Result := 8;
  end;
end;

function ForeignKindIsFloat(AKind: TForeignKind): Boolean;
begin
  Result := AKind in [fkFloat, fkDouble];
end;

end.
