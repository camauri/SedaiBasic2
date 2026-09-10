unit SedaiFastLookup;

{$mode objfpc}{$H+}
{$codepage UTF8}

// Two lookups that answer from a HASH INDEX instead of walking a list:
//   TIndexedStringList - a TStringList whose IndexOf and IndexOfName do not scan;
//   TObjectIndexMap    - "where in this list is this object", without TFPGObjectList.IndexOf.
//
// ⭐ TIndexedStringList.
//
// ⛔ WHY THIS EXISTS. The SSA generator keeps its facts in name registries - "which bank does this
// name use", "what capacity was declared for it", "what print form does it take" - and every one of
// them is a TStringList queried by IndexOf. TStringList.IndexOf is a LINEAR SCAN, and each step of it
// calls DoCompareText. So a program with N declarations pays N/2 string comparisons per lookup and
// does a lookup per use: the generator is QUADRATIC in the size of the program. Measured on fbc's
// compound/select_const2, at 16 macro groups: AnsiCompareText was 48% of the whole compile and grew
// 3.9x when the input doubled.
//
// ⭐ CORRECT BY CONSTRUCTION. Every mutation of a TStringList ends in Changed - that is what OnChange
// is for - so invalidating the index there cannot miss a writer. This is the trap the Superinstruction
// index paid for twice (a maintained index is exact only while every writer is known); here the writers
// are not enumerated at all, they are funnelled. An APPEND, the only shape that matters for a registry
// that is filled once and read many times, is then folded back in incrementally so the common case does
// not rebuild.
//
// ⛔ THE INDEX AND THE LIST MUST AGREE ON WHAT "EQUAL" MEANS. TStringList compares with AnsiCompareText
// (locale-aware) unless UseLocale is off; the index folds case the ASCII way. Rather than hope the two
// never disagree, the constructor turns UseLocale OFF, which makes the list's own comparison CompareText
// - exactly the fold the index uses. Where a caller re-enables UseLocale, sets CaseSensitive, or sorts
// the list, the fast path steps aside and the inherited scan answers.

interface

uses
  Classes, SysUtils, fgl;

type
  { ⛔ WHY TObjectIndexMap EXISTS. A CFG pass holds its per-block state in an array indexed by the
    block's POSITION, and its edges as object references - so every edge walk asks "where is this
    block", and TFPGObjectList.IndexOf answers by scanning. Dead-block elimination did that once per
    edge and TWICE per predecessor/successor it examined: on fbc's compound/select_const2 it was the
    single most expensive leaf of the whole optimiser. The list is not structurally changed while the
    pass reads it, so the position of every block can be learnt ONCE.
    ⚠️ Build it against a list that does not change underneath it. Rebuild after any Add/Delete. }
  TObjectIndexMap = class
  private
    FBucket: array of Integer;    // hash bucket -> first slot, -1 when empty
    FKey: array of Pointer;
    FValue: array of Integer;
    FNext: array of Integer;
    FCount: Integer;
    FMask: Integer;
    function Slot(P: Pointer): Integer; inline;
  public
    procedure Build(AList: TFPSList); overload;
    procedure Clear(ACapacity: Integer);
    procedure Add(P: Pointer; AIndex: Integer);
    function IndexOf(P: Pointer): Integer;   // -1 when the object is not in the list
  end;

  TIndexedStringList = class(TStringList)
  private
    FKeyBucket: array of Integer;   // hash bucket -> first entry index, -1 when empty
    FKeyNext: array of Integer;     // entry index -> next entry in the same bucket, -1 at the end
    FNameBucket: array of Integer;  // the same, over the NAME part (up to the name/value separator)
    FNameNext: array of Integer;
    FMask: Integer;                 // buckets - 1; buckets is always a power of two
    // ⛔⛔ TWO validities, not one, because the two indexes are invalidated by DIFFERENT writes.
    // Rewriting an entry's VALUE ("Values[name] := v" on a name already there) changes the whole
    // string - so the key index, which hashes the whole string, is stale - but it does NOT change
    // the NAME, so the name index is still exactly right. With one flag that write threw both away,
    // and the next IndexOfName rebuilt the entire table.
    // 📊 That is the preprocessor's shape, not a corner case: "__LINE__" is rewritten ONCE PER LINE,
    // so a header with thousands of #defines rebuilt a thousands-entry index on every line of source.
    // perf on win/shtypes.bi put 31% of the compile in IndexOfName, nearly all of it inside Rebuild.
    FKeyValid: Boolean;
    FNameValid: Boolean;
    function FoldedHash(const S: string; Len: Integer): LongWord;
    function NameLen(const S: string): Integer;
    procedure EnsureNextCapacity(ACount: Integer);
    procedure Rebuild;
    procedure IndexEntry(AIndex: Integer);
    function FastPath: Boolean;
  protected
    procedure Changed; override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    constructor Create;
    function IndexOf(const S: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
  end;

{ ⛔⛔ UpperCase E' IL COSTO PIU' DIFFUSO DELLA COMPILAZIONE, e per due ragioni distinte: la RTL
  ALLOCA sempre una stringa nuova, e la stringa che le passiamo e' spessissimo GIA' maiuscola perche'
  qualcuno l'ha gia' piegata a monte. perf su win/shlwapi.bi: InternalChangeCase 5,1% di SELF time, e
  a valle l'allocazione che ne segue alimenta Move, SysGetMem, ansistr_assign e decr_ref.
  ⭐ UpperFast fa una passata di LETTURA: se non c'e' una sola minuscola, restituisce LA STESSA
  stringa - nessuna allocazione, solo un refcount. Solo quando c'e' davvero qualcosa da cambiare
  alloca, e allora scrive con una tabella di 256 byte invece che con un test di appartenenza a un set.
  ⚠️ Equivalente byte per byte a SysUtils.UpperCase(ansistring), che in FPC piega SOLO 'a'..'z'
  (InternalChangeCase con l'insieme ASCII): nessuna dipendenza da locale, nessun UTF-8. }
function UpperFast(const S: string): string;

implementation

{ TObjectIndexMap }

function TObjectIndexMap.Slot(P: Pointer): Integer;
begin
  // Object pointers are allocation-aligned, so the low bits carry no information: fold the high ones
  // down before masking, or every key lands in a handful of buckets.
  Result := Integer((PtrUInt(P) shr 4) xor (PtrUInt(P) shr 20)) and FMask;
end;

procedure TObjectIndexMap.Clear(ACapacity: Integer);
var
  Buckets, i: Integer;
begin
  Buckets := 64;
  while Buckets < (ACapacity + 1) * 2 do Buckets := Buckets * 2;
  FMask := Buckets - 1;
  SetLength(FBucket, Buckets);
  for i := 0 to Buckets - 1 do FBucket[i] := -1;
  SetLength(FKey, ACapacity + 1);
  SetLength(FValue, ACapacity + 1);
  SetLength(FNext, ACapacity + 1);
  FCount := 0;
end;

procedure TObjectIndexMap.Add(P: Pointer; AIndex: Integer);
var
  h: Integer;
begin
  if FCount >= Length(FKey) then
  begin
    SetLength(FKey, FCount * 2 + 8);
    SetLength(FValue, FCount * 2 + 8);
    SetLength(FNext, FCount * 2 + 8);
  end;
  h := Slot(P);
  FKey[FCount] := P;
  FValue[FCount] := AIndex;
  FNext[FCount] := FBucket[h];       // newest first: a duplicate key answers the LAST index added
  FBucket[h] := FCount;
  Inc(FCount);
end;

procedure TObjectIndexMap.Build(AList: TFPSList);
var
  i: Integer;
begin
  Clear(AList.Count);
  // Backwards, because Add chains newest-first: this leaves the FIRST occurrence at the head of its
  // chain, which is the answer TFPGObjectList.IndexOf gives for a list holding the same item twice.
  for i := AList.Count - 1 downto 0 do
    Add(PPointer(AList.Items[i])^, i);
end;

function TObjectIndexMap.IndexOf(P: Pointer): Integer;
var
  i: Integer;
begin
  i := FBucket[Slot(P)];
  while i >= 0 do
  begin
    if FKey[i] = P then Exit(FValue[i]);
    i := FNext[i];
  end;
  Result := -1;
end;

var
  GUpperTab: array[0..255] of Char;

function UpperFast(const S: string): string;
var
  i, L: Integer;
  P: PChar;
begin
  L := Length(S);
  i := 1;
  while i <= L do
  begin
    if (S[i] >= 'a') and (S[i] <= 'z') then Break;
    Inc(i);
  end;
  if i > L then Exit(S);          // gia' maiuscola: nessuna allocazione, solo il refcount
  SetLength(Result, L);
  P := PChar(Result);
  // I byte prima del primo minuscolo sono gia' a posto: si copiano in blocco.
  if i > 1 then Move(PChar(S)^, P^, i - 1);
  while i <= L do
  begin
    P[i - 1] := GUpperTab[Byte(S[i])];
    Inc(i);
  end;
end;

procedure InitUpperTab;
var
  c: Integer;
begin
  for c := 0 to 255 do
    if (c >= Ord('a')) and (c <= Ord('z')) then GUpperTab[c] := Char(c - 32)
    else GUpperTab[c] := Char(c);
end;

{ TIndexedStringList }

constructor TIndexedStringList.Create;
begin
  inherited Create;
  // See the header: this is what makes the index and the list agree on "equal".
  UseLocale := False;
  FKeyValid := False;
  FNameValid := False;
  FMask := 0;
end;

function TIndexedStringList.FastPath: Boolean;
begin
  // Sorted lists already answer IndexOf by binary search, and a case-sensitive or locale-aware list
  // does not fold the way the index does. Either way, the inherited implementation is the truth.
  Result := (not Sorted) and (not CaseSensitive) and (not UseLocale);
end;

function TIndexedStringList.FoldedHash(const S: string; Len: Integer): LongWord;
// FNV-1a over the first Len bytes, folding a..z to A..Z - the same fold CompareText applies.
var
  i: Integer;
  c: Byte;
begin
  Result := 2166136261;
  for i := 1 to Len do
  begin
    c := Byte(S[i]);
    if (c >= Byte('a')) and (c <= Byte('z')) then Dec(c, 32);
    Result := (Result xor c) * 16777619;
  end;
end;

function TIndexedStringList.NameLen(const S: string): Integer;
// The length of the NAME part of a name=value entry. ⛔ An entry with NO separator answers -1, and that
// is not a detail: TStrings.IndexOfName computes the same "pos - 1" and skips the entry when it is
// negative, so an entry without a separator is invisible to IndexOfName. The index must be invisible in
// exactly the same places, or it would answer where the scan answers -1.
begin
  Result := Pos(NameValueSeparator, S) - 1;
end;

procedure TIndexedStringList.EnsureNextCapacity(ACount: Integer);
// The chain arrays are addressed by ENTRY INDEX, so they have to keep up with an append. Doubling keeps
// a fill loop amortised: without it every append past the last rebuild would throw the index away.
var
  n: Integer;
begin
  if ACount <= Length(FKeyNext) then Exit;
  n := Length(FKeyNext);
  if n < 64 then n := 64;
  while n < ACount do n := n * 2;
  SetLength(FKeyNext, n);
  SetLength(FNameNext, n);
end;

procedure TIndexedStringList.IndexEntry(AIndex: Integer);
// Chain one entry into both indexes, at the TAIL, so walking a chain visits entries in increasing
// order and the first match found is the lowest index - the answer IndexOf is defined to give.
var
  S: string;
  h, p, n: Integer;
begin
  S := Get(AIndex);
  EnsureNextCapacity(AIndex + 1);

  h := Integer(FoldedHash(S, Length(S)) and LongWord(FMask));
  if FKeyBucket[h] < 0 then
    FKeyBucket[h] := AIndex
  else
  begin
    p := FKeyBucket[h];
    while FKeyNext[p] >= 0 do p := FKeyNext[p];
    FKeyNext[p] := AIndex;
  end;
  FKeyNext[AIndex] := -1;

  FNameNext[AIndex] := -1;
  n := NameLen(S);
  if n >= 0 then                          // no separator: invisible to IndexOfName, so not indexed
  begin
    h := Integer(FoldedHash(S, n) and LongWord(FMask));
    if FNameBucket[h] < 0 then
      FNameBucket[h] := AIndex
    else
    begin
      p := FNameBucket[h];
      while FNameNext[p] >= 0 do p := FNameNext[p];
      FNameNext[p] := AIndex;
    end;
  end;
end;

procedure TIndexedStringList.Rebuild;
var
  Buckets, i: Integer;
begin
  // Room for one more append before the load factor bites, so a fill-then-read registry rebuilds
  // O(log n) times rather than once per entry.
  Buckets := 64;
  while Buckets < (Count + 1) * 2 do Buckets := Buckets * 2;
  FMask := Buckets - 1;
  SetLength(FKeyBucket, Buckets);
  SetLength(FNameBucket, Buckets);
  for i := 0 to Buckets - 1 do
  begin
    FKeyBucket[i] := -1;
    FNameBucket[i] := -1;
  end;
  EnsureNextCapacity(Count + 1);
  for i := 0 to Count - 1 do IndexEntry(i);
  FKeyValid := True;
  FNameValid := True;
end;

procedure TIndexedStringList.Changed;
begin
  FKeyValid := False;
  FNameValid := False;
  inherited Changed;
end;

procedure TIndexedStringList.InsertItem(Index: Integer; const S: string);
begin
  InsertItem(Index, S, nil);
end;

procedure TIndexedStringList.InsertItem(Index: Integer; const S: string; O: TObject);
var
  WasAppend, KeepKey, KeepName: Boolean;
begin
  // An APPEND leaves every existing index in place, so it can be folded into a live index instead of
  // throwing it away. Anything else (an insert in the middle) renumbers entries: let Changed drop it.
  // ⛔ EACH INDEX KEEPS ITS OWN VALIDITY THROUGH AN APPEND. Requiring BOTH to be valid looks
  // harmless and is not: one value rewrite ("Values[name] := v") retires the KEY index, and from that
  // moment every append refused to fold - so the name index was thrown away too and the next
  // IndexOfName rebuilt the whole table. That is the shape the preprocessor actually has, and it put
  // 11.8% of a header compile back into Rebuild.
  // ⚠️ Chaining the entry into an index that is already stale costs nothing and breaks nothing: the
  // first read of that index rebuilds it from zero, buckets included.
  KeepKey := FKeyValid;
  KeepName := FNameValid;
  WasAppend := (KeepKey or KeepName) and (Index = Count) and (Count < FMask);
  inherited InsertItem(Index, S, O);      // ...which calls Changed, clearing both flags
  if WasAppend then
  begin
    IndexEntry(Index);
    FKeyValid := KeepKey;
    FNameValid := KeepName;
  end;
end;

procedure TIndexedStringList.Put(Index: Integer; const S: string);
// Rewriting one entry in place. ⛔ The KEY index is over the whole string, so it is stale whatever
// changed; the NAME index is stale only if the NAME did - and the write that actually happens in a
// hot loop, "Values[name] := newvalue", leaves the name exactly where it was.
// ⚠️ The name is compared the way the index folds it (CompareText, no locale), so a caller writing
// the name in a different case is still the same entry to both.
var
  KeepName: Boolean;
  Old: string;
  nOld, nNew: Integer;
begin
  KeepName := False;
  if FNameValid and (Index >= 0) and (Index < Count) then
  begin
    Old := Get(Index);
    nOld := NameLen(Old);
    nNew := NameLen(S);
    KeepName := (nOld >= 0) and (nOld = nNew) and (CompareText(Copy(Old, 1, nOld), Copy(S, 1, nNew)) = 0);
    // ⭐ ...AND A ROW REWRITTEN WITHOUT A SEPARATOR IS A TOMBSTONE, which the name index survives too.
    // An entry with no NameValueSeparator is invisible to IndexOfName BY CONSTRUCTION - the scan skips
    // it and so does this index - so the old name's chain link simply stops matching. That is what lets
    // a caller RETIRE a name (the preprocessor's "#undef") without deleting a row: deleting renumbers
    // every entry above it and costs the whole index, and a Windows header undefines hundreds of names.
    // 📊 perf on win/shlwapi.bi: 48% of the compile was IndexOfName -> Rebuild, one rebuild per #undef.
    if nNew < 0 then KeepName := True;
  end;
  inherited Put(Index, S);      // ...which calls Changed, clearing both flags
  if KeepName then FNameValid := True;
end;

procedure TIndexedStringList.PutObject(Index: Integer; AObject: TObject);
var
  Keep: Boolean;
begin
  // Only the OBJECT changes; the strings the index is built over do not. Writing Objects[i] is how a
  // registry records its fact, so invalidating here would defeat the index on exactly the hot pattern.
  Keep := FKeyValid and FNameValid;
  inherited PutObject(Index, AObject);
  FKeyValid := Keep;
  FNameValid := Keep;
end;

function TIndexedStringList.IndexOf(const S: string): Integer;
var
  i: Integer;
begin
  if not FastPath then Exit(inherited IndexOf(S));
  if not FKeyValid then Rebuild;
  i := FKeyBucket[Integer(FoldedHash(S, Length(S)) and LongWord(FMask))];
  while i >= 0 do
  begin
    if CompareText(Get(i), S) = 0 then Exit(i);
    i := FKeyNext[i];
  end;
  Result := -1;
end;

function TIndexedStringList.IndexOfName(const Name: string): Integer;
var
  i, L: Integer;
  E: string;
begin
  if not FastPath then Exit(inherited IndexOfName(Name));
  if not FNameValid then Rebuild;
  L := Length(Name);
  i := FNameBucket[Integer(FoldedHash(Name, L) and LongWord(FMask))];
  while i >= 0 do
  begin
    E := Get(i);
    if (NameLen(E) = L) and (CompareText(Copy(E, 1, L), Name) = 0) then Exit(i);
    i := FNameNext[i];
  end;
  Result := -1;
end;

initialization
  InitUpperTab;

end.
