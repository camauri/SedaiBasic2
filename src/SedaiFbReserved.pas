unit SedaiFbReserved;

// The names fbc keeps for itself (DIVERGENZE 590): the MEASURED table of SedaiFbReservedNames.inc, asked by the
// preprocessor (a #define or #macro may not take one) and by the SSA (a variable may not). One table, two readers.

{$mode objfpc}{$H+}

interface

{ True when UName (already upper case) is a name fbc 1.10.1 refuses to redefine as a MACRO. }
function IsFbReservedName(const UName: string): Boolean;
{ ...and as a VARIABLE: the same set measured again with "Dim As Integer NAME" and "Dim As UByte NAME(0 To 3)" (both
  forms answer alike), and it is not quite the same set - CVA_LIST and __FUNCTION_NQ__ may name a variable. }
function IsFbReservedVarName(const UName: string): Boolean;

implementation

uses SysUtils;

{$I SedaiFbReservedNames.inc}

function IsFbReservedName(const UName: string): Boolean;
// Binary search: the table is sorted in ordinal order.
var
  lo, hi, mid, c: Integer;
begin
  lo := Low(FB_RESERVED_MACRO_NAMES); hi := High(FB_RESERVED_MACRO_NAMES);
  while lo <= hi do
  begin
    mid := (lo + hi) div 2;
    c := CompareStr(FB_RESERVED_MACRO_NAMES[mid], UName);
    if c = 0 then Exit(True);
    if c < 0 then lo := mid + 1 else hi := mid - 1;
  end;
  Result := False;
end;

function IsFbReservedVarName(const UName: string): Boolean;
begin
  if (UName = 'CVA_LIST') or (UName = '__FUNCTION_NQ__') then Exit(False);
  // ⛔ NOT TRUE and FALSE, though fbc refuses "Dim As Integer True": they are intrinsic DEFINES, and after
  // "#undef TRUE" fbc takes "Const TRUE = -1" - two tests of its own suite do exactly that. While they are defined the
  // lexer here refuses them already; once undefined they are free. (The caller skips every #undef'd name as well.)
  Result := IsFbReservedName(UName);
end;

end.
