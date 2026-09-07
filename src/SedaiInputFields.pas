unit SedaiInputFields;
{$mode objfpc}{$H+}
{ The FIELD rule of INPUT, in one place for its two readers.

  INPUT # (SedaiFileIO) and console INPUT (the VM) read the same grammar - fbc uses one field
  reader for both, and so must this project or the two drift: they had, and DIVERGENZE 169 (the
  file) and 170 (the console) were the same rule missing twice. The VM cannot use SedaiFileIO
  (SedaiFileIO uses the VM), so the rule lives here, below both. }
interface

{ An INPUT FIELD, once the scanner has cut it out of the line - measured against fbc 7 Sep 2026
  over fourteen forms (DIVERGENZE 169):
    * LEADING blanks and tabs are not part of the field: "42, text" reads `text`;
    * TRAILING blanks ARE: "  a  ,  b  " reads `a  ` and `b  `;
    * a field that OPENS with a quote is the text up to the closing quote, commas included, and
      the quotes themselves go; blanks after the closing quote go too;
    * what follows the closing quote is the NEXT field, not this one: `"x"y, z` reads x, y, z. So
      the helper reports how many units of the raw field it did not consume, and the caller puts
      the stream back at their start - the delimiter it had swallowed is then read by the next field.
  Works on the raw UNITS of the encoding (UW bytes each, little-endian) so the byte and the wide
  scanner share it: a blank, a tab and a quote are ASCII in every encoding this reader knows. }
procedure TrimInputField(var Raw: string; UW: Integer; out TailUnits: Integer);

{ The next field of a BYTE line, starting at Pos (1-based): cut at the first comma outside quotes
  or at the end, apply TrimInputField, and leave Pos on the character after the delimiter - or on
  the first unit of the tail after a closing quote, which is where the next field begins. At the
  end of the line the result is '' and Pos stays past the end, which is how a caller knows to read
  another line: the fields are a STREAM, and a line break is a delimiter like a comma. }
function NextInputField(const Line: string; var Pos: Integer): string;

implementation

{ (the rule is stated in the interface)
  An INPUT # FIELD, once the scanner has cut it out of the line - measured against fbc 7 Sep 2026
  over fourteen forms (DIVERGENZE 169):
    * LEADING blanks and tabs are not part of the field: "42, text" reads `text`;
    * TRAILING blanks ARE: "  a  ,  b  " reads `a  ` and `b  `;
    * a field that OPENS with a quote is the text up to the closing quote, commas included, and
      the quotes themselves go; blanks after the closing quote go too;
    * what follows the closing quote is the NEXT field, not this one: `"x"y, z` reads x, y, z. So
      the helper reports how many units of the raw field it did not consume, and the caller puts
      the stream back at their start - the delimiter it had swallowed is then read by the next field.
  Works on the raw UNITS of the encoding (UW bytes each, little-endian) so the byte and the wide
  scanner share it: a blank, a tab and a quote are ASCII in every encoding this reader knows. }
procedure TrimInputField(var Raw: string; UW: Integer; out TailUnits: Integer);
var
  NUnits, I, J, K: Integer;
  function UnitCode(Idx: Integer): Integer;   // 0-based unit -> its code, -1 when not ASCII
  var B: Integer;
  begin
    Result := Ord(Raw[Idx * UW + 1]);
    for B := 2 to UW do
      if Ord(Raw[Idx * UW + B]) <> 0 then Exit(-1);
  end;
  function IsBlank(Idx: Integer): Boolean;
  begin
    Result := UnitCode(Idx) in [32, 9];
  end;
begin
  TailUnits := 0;
  if UW < 1 then UW := 1;
  NUnits := Length(Raw) div UW;
  I := 0;
  while (I < NUnits) and IsBlank(I) do Inc(I);
  if (I < NUnits) and (UnitCode(I) = Ord('"')) then
  begin
    J := I + 1;
    while (J < NUnits) and (UnitCode(J) <> Ord('"')) do Inc(J);
    // J is the closing quote, or NUnits when the line ended first (then the field runs to its end)
    K := J + 1;
    while (K < NUnits) and IsBlank(K) do Inc(K);
    if K < NUnits then TailUnits := NUnits - K;
    Raw := Copy(Raw, (I + 1) * UW + 1, (J - I - 1) * UW);
  end
  else if I > 0 then
    Raw := Copy(Raw, I * UW + 1, (NUnits - I) * UW);
end;

function NextInputField(const Line: string; var Pos: Integer): string;
var
  Start, I, Tail: Integer;
  InQ: Boolean;
begin
  Result := '';
  if Pos < 1 then Pos := 1;
  if Pos > Length(Line) then Exit;
  Start := Pos;
  I := Start;
  InQ := False;
  while I <= Length(Line) do
  begin
    if Line[I] = '"' then InQ := not InQ
    else if (Line[I] = ',') and not InQ then Break;
    Inc(I);
  end;
  Result := Copy(Line, Start, I - Start);
  TrimInputField(Result, 1, Tail);
  if Tail > 0 then Pos := I - Tail          // the next field starts at the tail, the comma is its
  else Pos := I + 1;                        // past the comma (or past the end of the line)
end;

end.
