unit SedaiBigInt;

{ ============================================================================
  Aritmetica multipla - il NUCLEO, senza superficie di linguaggio.

  Piano: job/docs/PIANO_BIGINT.md. Riferimento di correttezza e di velocita':
  job/tests/bench/pid64_proto.pas (pidigits completo, MATCH con l'oracolo a
  1000 cifre).

  ⭐ PERCHE' ESISTE, in un numero: pidigits con l'aritmetica in BASIC sta a
  18,9x da GMP; con l'aritmetica qui dentro il prototipo misura 2,5x - e a
  N=1000 batte GMP. Il premio e' 7,7-10,7x sul nostro AOT, e si scompone in
  due meta' che vanno prese ENTRAMBE: 3,0x dal togliere l'interpretazione PER
  LIMB (quindi l'intero ciclo sta qui, non una primitiva per limb) e 4,3x dalla
  base 2^64, che BASIC non puo' esprimere perche' a(i)*k trabocca in Int64.

  ⭐ LA RAPPRESENTAZIONE, e perche' NON e' una stringa. Le stringhe di questo
  runtime sono AnsiString di FPC, quindi avrebbero dato riconteggio e copia su
  scrittura gratis - ed e' stata la prima proposta. E' stata scartata: se un
  BigInt FOSSE una stringa, ogni consumatore che chiede "cosa c'e' in questo
  registro stringa?" (banco, spill, chiamate foglia AOT/JIT, PRINT, confronti,
  LEN) vedrebbe dei BigInt senza saperlo. E' la malattia che PIANO_TIPI.md ha
  appena curato - un record era tre vettori per banco, quindi un offset in byte
  non esisteva come concetto. Una cosa che ne porta due.
  ⇒ Tipo PROPRIO, e la copia su scrittura costa DIECI RIGHE (UniqueLimbs):
  misurato 0 ms su 2000 chiamate quando non e' condiviso, contro le stesse
  cifre della stringa quando lo e'.

  ⛔ INVARIANTE, e vale per ogni funzione qui dentro: una lunghezza descrive
  sempre una magnitudine NORMALIZZATA - niente limb zero in testa - perche' il
  confronto guarda la lunghezza per prima e uno zero in testa farebbe risultare
  maggiore il numero piu' piccolo. Chi accorcia, normalizza.
  ============================================================================ }

{$mode objfpc}{$H+}{$asmmode att}

interface

type
  { I limb, dal meno significativo. La lunghezza in limb e' Length(); la
    normalizzazione e' un invariante, non una cortesia. }
  TLimbs = array of QWord;

{ 64x64 -> i 64 bit ALTI del prodotto: la via veloce di questa piattaforma. }
function MulHi64(a, b: QWord): QWord; {$IFDEF CPUX86_64}inline;{$ENDIF}

{ ⛔ La stessa cosa in Pascal puro, ed e' SEMPRE compilata anche dove non serve.
  Non e' codice morto: e' l'ORACOLO con cui si verifica la via veloce. Messa in
  un ramo {$ELSE} sarebbe sparita da x86-64, e il controllo che la confronta
  sarebbe diventato "assembly contro se stesso" - verde e cieco. }
function MulHi64Portable(a, b: QWord): QWord;

{ Sgancia SOLO se condiviso: l'equivalente di UniqueString per un array
  dinamico, che di suo NON fa copia su scrittura (a2 := a1; a2[0] := 9 cambia
  anche a1[0] - misurato, non dedotto). }
procedure UniqueLimbs(var a: TLimbs);

procedure BigSetSmall(var a: TLimbs; var n: Integer; v: QWord);
procedure BigCopy(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer);
{ a *= k, con k che sta in un limb. Riporta il numero di limb usati. }
procedure BigMulSmall(var a: TLimbs; var n: Integer; k: QWord);
procedure BigAdd(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
{ dst = a - b, solo magnitudini: il chiamante garantisce a >= b. }
procedure BigSub(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
function BigCmp(const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer): Integer;

implementation

{ ⛔ Il prodotto a 128 bit e' l'unica cosa qui che il linguaggio ospite non sa
  esprimere, ed e' anche l'unica che tenta l'assembly. Due vie:
  - x86-64: una MUL, ma l'assembly VA GATED PER SISTEMA. La cicatrice e' del
    12 ago 2026: VecScanPrefix era scritta per la sola convenzione Win64 e
    protetta da {$IFDEF CPUX86_64} - l'ARCHITETTURA, non il SISTEMA - e su
    Linux leggeva gli argomenti dai registri sbagliati, sbagliando ~meta' delle
    volte a seconda dell'ASLR. Qui la funzione e' scritta in Pascal e il
    compilatore mette gli argomenti dove vuole lui: nessuna convenzione da
    indovinare, e nessun ramo per sistema operativo.
  - ovunque: quattro prodotti a 32 bit. Piu' lento, ma il target WASM e un
    eventuale ARM non restano fuori - ed e' anche l'oracolo con cui la via
    veloce si verifica. }
function MulHi64Portable(a, b: QWord): QWord;
var
  lo: QWord;
begin
  Result := ((a shr 32) * (b shr 32)) +
            (((a and $FFFFFFFF) * (b shr 32)) shr 32) +
            (((a shr 32) * (b and $FFFFFFFF)) shr 32);
  lo := (((a and $FFFFFFFF) * (b and $FFFFFFFF)) shr 32) +
        (((a and $FFFFFFFF) * (b shr 32)) and $FFFFFFFF) +
        (((a shr 32) * (b and $FFFFFFFF)) and $FFFFFFFF);
  Result := Result + (lo shr 32);
end;

{$IFDEF CPUX86_64}
function MulHi64(a, b: QWord): QWord; inline;
begin
  { ⭐ L'assembly riferisce i PARAMETRI PER NOME: e' FPC a sapere dove li ha
    messi, quindi non c'e' nessuna convenzione di chiamata da indovinare, e il
    gate {$IFDEF CPUX86_64} - l'ARCHITETTURA - qui e' quello GIUSTO, perche'
    l'unica cosa che presuppone e' che esista l'istruzione MUL.
    ⛔ E' esattamente cio' che VecScanPrefix NON faceva: la' i registri erano
    cablati a mano su Win64 sotto lo stesso gate di architettura, e su System V
    la funzione leggeva gli argomenti dai registri sbagliati - meta' delle volte
    la risposta era sbagliata, a seconda dell'ASLR. Vedi
    job/tests/bas/bug_regex_vecfilter.bas.
    📊 Vale 1,61x su pidigits N=3000 rispetto alla via portabile. }
  asm
    movq a, %rax
    mulq b
    movq %rdx, Result
  end;
end;
{$ELSE}
function MulHi64(a, b: QWord): QWord;
begin
  Result := MulHi64Portable(a, b);
end;
{$ENDIF}

{ Il refcount di un array dinamico sta due PtrInt prima dei dati (refcount,
  poi high). ⚠️ E' un dettaglio implementativo di FPC e sta QUI, in una
  funzione sola, per non spargerlo. }
function LimbsRefCount(const a: TLimbs): PtrInt;
begin
  if a = nil then Exit(0);
  Result := PPtrInt(PtrUInt(Pointer(a)) - 2 * SizeOf(PtrInt))^;
end;

procedure UniqueLimbs(var a: TLimbs);
begin
  if (a <> nil) and (LimbsRefCount(a) > 1) then
    a := Copy(a, 0, Length(a));
end;

procedure BigSetSmall(var a: TLimbs; var n: Integer; v: QWord);
begin
  UniqueLimbs(a);
  if Length(a) < 1 then SetLength(a, 1);
  a[0] := v;
  n := 1;
end;

procedure BigCopy(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer);
var
  i: Integer;
begin
  UniqueLimbs(dst);
  if Length(dst) < an then SetLength(dst, an);
  for i := 0 to an - 1 do dst[i] := a[i];
  dn := an;
end;

procedure BigMulSmall(var a: TLimbs; var n: Integer; k: QWord);
var
  i: Integer;
  lo, hi, t, carry: QWord;
begin
  UniqueLimbs(a);
  carry := 0;
  for i := 0 to n - 1 do
  begin
    lo := a[i] * k;
    hi := MulHi64(a[i], k);
    t := lo + carry;
    if t < lo then Inc(hi);          { il riporto dell'addizione entra nella parte alta }
    a[i] := t;
    carry := hi;
  end;
  while carry > 0 do
  begin
    if n >= Length(a) then SetLength(a, n + 8);
    a[n] := carry;
    carry := 0;                       { un limb basta: hi < 2^64 }
    Inc(n);
  end;
  { k = 0 azzera tutto: normalizzare, o un confronto per lunghezza mentirebbe. }
  while (n > 1) and (a[n - 1] = 0) do Dec(n);
end;

procedure BigAdd(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
var
  i, m: Integer;
  s, carry, x: QWord;
begin
  m := an; if bn > m then m := bn;
  UniqueLimbs(dst);
  if Length(dst) < m + 1 then SetLength(dst, m + 1);
  carry := 0;
  for i := 0 to m - 1 do
  begin
    s := carry; carry := 0;
    if i < an then begin x := s + a[i]; if x < s then carry := 1; s := x; end;
    if i < bn then begin x := s + b[i]; if x < s then Inc(carry); s := x; end;
    dst[i] := s;
  end;
  dn := m;
  if carry > 0 then begin dst[dn] := carry; Inc(dn); end;
  while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
end;

procedure BigSub(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
var
  i: Integer;
  bb, v, borrow: QWord;
begin
  UniqueLimbs(dst);
  if Length(dst) < an then SetLength(dst, an);
  borrow := 0;
  for i := 0 to an - 1 do
  begin
    if i < bn then bb := b[i] else bb := 0;
    v := a[i] - bb;
    if a[i] < bb then
    begin
      dst[i] := v - borrow;
      borrow := 1;
    end
    else if v < borrow then
    begin
      dst[i] := v - borrow;
      borrow := 1;
    end
    else
    begin
      dst[i] := v - borrow;
      borrow := 0;
    end;
  end;
  dn := an;
  while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
end;

function BigCmp(const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer): Integer;
var
  i: Integer;
begin
  if an <> bn then
  begin
    if an < bn then Exit(-1) else Exit(1);
  end;
  for i := an - 1 downto 0 do
    if a[i] <> b[i] then
    begin
      if a[i] < b[i] then Exit(-1) else Exit(1);
    end;
  Result := 0;
end;

end.
