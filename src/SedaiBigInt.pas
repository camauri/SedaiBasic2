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

{ ⛔⛔ L'AVVOLGIMENTO E' L'ALGORITMO, non un incidente. Il riporto di
  un'addizione fra limb si rileva PROPRIO dal trabocco (`s := a + b;
  if s < b then riporto`), e la parte bassa di un prodotto e' il
  risultato troncato a 64 bit. Con {$Q+} - che la build di debug
  attiva - ognuna di quelle operazioni solleva EIntOverflow, e il
  programma muore su un'aritmetica CORRETTA.
  Trovato il 14 ago 2026: in rilascio andava, in debug no, e il
  sintomo arrivava come un errore alla riga BASIC sbagliata.
  ⚠️ Vale per l'intera unita': ogni funzione qui dentro lavora su
  magnitudini senza segno e conta sul modulo 2^64. }
{$Q-}{$R-}

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

{ (hi:lo) div d, col resto in rem. ⛔ RICHIEDE hi < d, che è l'invariante del passo
  scolastico che la usa: senza, il quoziente non starebbe in 64 bit.
  ⚠️ È la divisione lunga BIT A BIT, 64 giri: lenta di proposito. Serve solo alla
  conversione in decimale, che non sta su nessun percorso caldo (pidigits fa uscire
  le cifre dal rubinetto una alla volta e non converte mai un numero intero). La via
  x86 sarebbe una sola `divq`, ma quella va scritta in assembly e l'assembly qui si
  paga in convenzioni di chiamata: si aggiunge quando una misura dirà che serve. }
function DivMod128By64(hi, lo, d: QWord; out rem: QWord): QWord;

{$IFDEF CPUX86_64}
{ I tre cicli caldi in assembly. Esportati perche' il controllo che li confronta con la
  via portabile e' un programma a parte: una primitiva che nessuno puo' verificare da
  fuori e' una primitiva di cui nessuno sa se e' giusta. }
function MulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
function AddLimbRun(pd, pa, pb: Pointer; n: PtrInt): QWord;
function SubLimbRun(pd, pa, pb: Pointer; n: PtrInt): QWord;
function AddMulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
function SubMulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
{$ENDIF}

{ Sgancia SOLO se condiviso: l'equivalente di UniqueString per un array
  dinamico, che di suo NON fa copia su scrittura (a2 := a1; a2[0] := 9 cambia
  anche a1[0] - misurato, non dedotto). }
procedure UniqueLimbs(var a: TLimbs);
{ Il riconteggio di un vettore di limb: serve a decidere se si puo' scrivere in posto. }
function LimbsRefCount(const a: TLimbs): PtrInt;

procedure BigSetSmall(var a: TLimbs; var n: Integer; v: QWord);
procedure BigCopy(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer);
{ a *= k, con k che sta in un limb. Riporta il numero di limb usati. }
procedure BigMulSmall(var a: TLimbs; var n: Integer; k: QWord);
{ dst = a * k in UNA passata, senza copiare prima. ⭐ BigMulSmall lavora in posto, quindi
  un destinatario diverso dalla sorgente costava una COPIA piu' una moltiplicazione: due
  passate sull'intero numero dove ne basta una. E' la forma di "probe = den * q", che il
  ciclo delle cifre esegue fino a dieci volte per cifra.
  ⚠️ dst puo' coincidere con a: si legge a[i] e si scrive dst[i], stesso indice. }
procedure BigMulSmallTo(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; k: QWord);
{ a += k, con k che sta in un limb. La coppia con BigMulSmall e' quanto basta a fare
  Horner in base 10^19, cioe' a leggere un numero decimale. }
procedure BigAddSmall(var a: TLimbs; var n: Integer; k: QWord);
{ dst = a * b, solo magnitudini. ⚠️ dst PUO' essere a o b senza danno: il prodotto si
  costruisce in un vettore a parte e viene consegnato alla fine, perche' lo schema
  scolastico legge a[i] e b[j] mentre scrive in i+j, e con l'aliasing leggerebbe
  cifre gia' sovrascritte. Costa una allocazione e toglie una classe intera di bachi. }
procedure BigMul(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
procedure BigAdd(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
{ dst = a - b, solo magnitudini: il chiamante garantisce a >= b. }
procedure BigSub(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
{ q = a div b, r = a mod b, solo MAGNITUDINI. b <> 0 e' responsabilita' del chiamante.
  ⭐ E' l'algoritmo D di Knuth (TAOCP 4.3.1): normalizzazione, stima del quoziente da
  DUE limb del dividendo su UNO del divisore, correzione della stima, moltiplica-e-
  sottrai, e il raro "somma indietro" quando la stima era alta di uno.
  ⚠️ q e r NON possono essere a o b: si costruiscono a parte e si consegnano. }
{ ⚠️ wu e wv sono SPAZIO DI LAVORO del chiamante, non risultati: la normalizzazione di
  Knuth costruisce un dividendo e un divisore spostati, e allocarli a ogni chiamata
  costa piu' dell'algoritmo su numeri di migliaia di limb (misurato: la divisione
  risultava PIU' LENTA del ciclo di prove che doveva sostituire). Chi chiama li tiene
  e li ripassa; crescono una volta e poi non piu'. }
procedure BigDivMod(var q: TLimbs; var qn: Integer; var r: TLimbs; var rn: Integer;
                    const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer;
                    var wu, wv: TLimbs);

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
{$IFDEF CPUX86_64}
function MulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
{ dst[0..n-1] = a[0..n-1] * k, e riporta il riporto uscente. ⭐ E' il ciclo piu' caldo di
  tutta l'aritmetica multipla, e in Pascal costava ~4 cicli per limb perche' il riporto
  si rileva con un CONFRONTO e un salto. Qui la MULQ produce rdx:rax e la ADC prende il
  riporto dai flag: nessun ramo, nessuna dipendenza inventata.
  ⛔ I parametri si riferiscono PER NOME e si copiano subito in registri CHIAMANTE-salvati
  (r8-r11, rax, rcx, rdx): toccare rbx o r12-r15 senza salvarli romperebbe il chiamante, e
  cablare rdi/rsi presupporrebbe una convenzione di chiamata - l'errore che VecScanPrefix
  ha pagato per mezza giornata. Qui FPC decide dove stanno e l'assembly non lo presuppone. }
label
  giro, fine;
var
  res: QWord;
begin
  asm
    movq pd, %r8
    movq pa, %r9
    movq n,  %r10
    movq k,  %r11
    xorq %rcx, %rcx
    testq %r10, %r10
    jz   fine
  giro:
    movq (%r9), %rax
    mulq %r11
    addq %rcx, %rax
    adcq $0, %rdx
    movq %rax, (%r8)
    movq %rdx, %rcx
    addq $8, %r9
    addq $8, %r8
    decq %r10
    jnz  giro
  fine:
    movq %rcx, res
  end;
  Result := res;
end;
{$ENDIF}

{$IFDEF CPUX86_64}
function AddLimbRun(pd, pa, pb: Pointer; n: PtrInt): QWord;
{ dst[0..n-1] = a[0..n-1] + b[0..n-1], riporta il riporto uscente.
  ⭐ LA CATENA ADC vive nel flag di riporto, quindi il ciclo non puo' toccare i flag fra
  un limb e il successivo. Il trucco e' l'INDICE NEGATIVO: si parte da -n e si sale con
  INC, che a differenza di ADD/SUB **non modifica CF** - tocca solo ZF, che serve al
  salto. Senza questo si dovrebbe salvare e ripristinare il riporto a ogni giro, ed e'
  esattamente cio' che rende lento il ciclo scritto in Pascal. }
label giro, fine;
var
  res: QWord;
begin
  asm
    movq pd, %r8
    movq pa, %r9
    movq pb, %r10
    movq n,  %rcx
    testq %rcx, %rcx
    jz   fine
    { i puntatori si spostano alla FINE e l'indice sale da -n a 0 }
    leaq (%r8,%rcx,8), %r8
    leaq (%r9,%rcx,8), %r9
    leaq (%r10,%rcx,8), %r10
    negq %rcx
    clc
  giro:
    movq (%r9,%rcx,8), %rax
    adcq (%r10,%rcx,8), %rax
    movq %rax, (%r8,%rcx,8)
    incq %rcx            { INC non tocca CF: la catena sopravvive }
    jnz  giro
  fine:
    movq $0, %rax
    adcq $0, %rax
    movq %rax, res
  end;
  Result := res;
end;

function SubLimbRun(pd, pa, pb: Pointer; n: PtrInt): QWord;
{ dst = a - b sugli n limb comuni; riporta il PRESTITO uscente. Stessa forma. }
label giro, fine;
var
  res: QWord;
begin
  asm
    movq pd, %r8
    movq pa, %r9
    movq pb, %r10
    movq n,  %rcx
    testq %rcx, %rcx
    jz   fine
    leaq (%r8,%rcx,8), %r8
    leaq (%r9,%rcx,8), %r9
    leaq (%r10,%rcx,8), %r10
    negq %rcx
    clc
  giro:
    movq (%r9,%rcx,8), %rax
    sbbq (%r10,%rcx,8), %rax
    movq %rax, (%r8,%rcx,8)
    incq %rcx
    jnz  giro
  fine:
    movq $0, %rax
    adcq $0, %rax
    movq %rax, res
  end;
  Result := res;
end;
{$ENDIF}

{$IFDEF CPUX86_64}
function AddMulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
{ dst[0..n-1] += a[0..n-1] * k, riporta la parte che resta da propagare. E' mpn_addmul_1,
  il ciclo interno del prodotto scolastico.
  ⚠️ Qui il riporto sta in un REGISTRO, non nei flag - lo si ricostruisce a ogni giro con
  due ADC - quindi il controllo del ciclo puo' usare liberamente ADD e DEC: la cautela
  dell'indice negativo serve solo dove la catena vive nel flag. }
label giro, fine;
var
  res: QWord;
begin
  asm
    movq pd, %r8
    movq pa, %r9
    movq n,  %r10
    movq k,  %r11
    xorq %rcx, %rcx
    testq %r10, %r10
    jz   fine
  giro:
    movq (%r9), %rax
    mulq %r11
    addq %rcx, %rax
    adcq $0, %rdx
    addq %rax, (%r8)      { somma nel destinatario: il trabocco finisce in CF }
    adcq $0, %rdx
    movq %rdx, %rcx
    addq $8, %r9
    addq $8, %r8
    decq %r10
    jnz  giro
  fine:
    movq %rcx, res
  end;
  Result := res;
end;

function SubMulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
{ dst[0..n-1] -= a[0..n-1] * k, riporta la parte che resta da SOTTRARRE piu' in alto.
  E' mpn_submul_1, il moltiplica-e-sottrai dell'algoritmo D. }
label giro, fine;
var
  res: QWord;
begin
  asm
    movq pd, %r8
    movq pa, %r9
    movq n,  %r10
    movq k,  %r11
    xorq %rcx, %rcx
    testq %r10, %r10
    jz   fine
  giro:
    movq (%r9), %rax
    mulq %r11
    addq %rcx, %rax
    adcq $0, %rdx
    subq %rax, (%r8)      { sottrae: il prestito finisce in CF }
    adcq $0, %rdx
    movq %rdx, %rcx
    addq $8, %r9
    addq $8, %r8
    decq %r10
    jnz  giro
  fine:
    movq %rcx, res
  end;
  Result := res;
end;
{$ENDIF}

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

function DivMod128By64(hi, lo, d: QWord; out rem: QWord): QWord;
{$IFDEF CPUX86_64}
var
  q, r: QWord;
begin
  { ⭐ Una sola DIVQ. Come in MulHi64, l'assembly riferisce i nomi e lascia a FPC il
    compito di sapere dove stanno: nessuna convenzione di chiamata da indovinare, e il
    gate {$IFDEF CPUX86_64} e' quello giusto perche' l'unica cosa che presuppone e'
    l'esistenza dell'istruzione.
    ⛔ Il quoziente e il resto passano da VARIABILI LOCALI, non dal parametro `out`: per
    un parametro per riferimento il nome in assembly designa il PUNTATORE, non il posto
    dove scrivere, e sbagliarlo qui vorrebbe dire scrivere sopra un indirizzo.
    ⛔ DIVQ solleva #DE se il quoziente non sta in 64 bit: la precondizione hi < d NON e'
    una cortesia, e' cio' che tiene in piedi questa funzione. L'algoritmo D la garantisce.
    📊 Serve al ciclo interno della divisione lunga: la via bit a bit qui sotto fa 64 giri
    per limb ed e' quella che rendeva impraticabile una divisione vera. }
  asm
    movq hi, %rdx
    movq lo, %rax
    divq d
    movq %rax, q
    movq %rdx, r
  end;
  rem := r;
  Result := q;
end;
{$ELSE}
var
  i: Integer;
  q: QWord;
  carry: Boolean;
begin
  q := 0;
  rem := hi;
  for i := 63 downto 0 do
  begin
    { ⛔ Il bit ALTO va salvato PRIMA dello shift: rem può valere fino a d-1, e con
      d oltre 2^63 lo shift lo perde. Se è uscito, il valore vero è rem + 2^64 ed è
      certamente >= d, quindi si sottrae comunque - e la sottrazione modulo 2^64 dà
      il risultato giusto. Senza questa riga la conversione sbaglia solo sui numeri
      grandi, che è il modo peggiore di sbagliare. }
    carry := (rem shr 63) <> 0;
    rem := (rem shl 1) or ((lo shr i) and 1);
    q := q shl 1;
    if carry or (rem >= d) then
    begin
      rem := rem - d;
      q := q or 1;
    end;
  end;
  Result := q;
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
  {$IFDEF CPUX86_64}
  { stesso ciclo di BigMulSmallTo, con destinazione = sorgente }
  carry := MulLimbRun(@a[0], @a[0], n, k);
  {$ELSE}
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
  {$ENDIF}
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

procedure BigMulSmallTo(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; k: QWord);
var
  i: Integer;
  lo, hi, t, carry: QWord;
begin
  if (an <= 0) or (k = 0) then
  begin
    UniqueLimbs(dst);
    if Length(dst) < 1 then SetLength(dst, 1);
    dst[0] := 0; dn := 1;
    Exit;
  end;
  UniqueLimbs(dst);
  if Length(dst) < an + 1 then SetLength(dst, an + 1);
  {$IFDEF CPUX86_64}
  carry := MulLimbRun(@dst[0], @a[0], an, k);
  {$ELSE}
  carry := 0;
  for i := 0 to an - 1 do
  begin
    lo := a[i] * k;
    hi := MulHi64(a[i], k);
    t := lo + carry;
    if t < lo then Inc(hi);
    dst[i] := t;          { stesso indice letto e scritto: dst = a e' innocuo }
    carry := hi;
  end;
  {$ENDIF}
  dn := an;
  if carry > 0 then begin dst[dn] := carry; Inc(dn); end;
  while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
end;

procedure BigAddSmall(var a: TLimbs; var n: Integer; k: QWord);
var
  i: Integer;
  s: QWord;
  carry: QWord;
begin
  UniqueLimbs(a);
  carry := k;
  i := 0;
  while (carry <> 0) and (i < n) do
  begin
    s := a[i] + carry;
    if s < carry then carry := 1 else carry := 0;   { il trabocco E' il riporto }
    a[i] := s;
    Inc(i);
  end;
  if carry <> 0 then
  begin
    if n >= Length(a) then SetLength(a, n + 8);
    a[n] := carry;
    Inc(n);
  end;
end;

{ ⭐⭐⭐ KARATSUBA. Il prodotto scolastico e' O(n^2): nessuna qualita' di assembly
  recupera un ordine di complessita', e sopra qualche decina di limb e' li' che le
  librerie serie guadagnano. Spezzando a = a1*B^m + a0 e b = b1*B^m + b0 servono TRE
  prodotti di meta' taglia invece di quattro:
      z0 = a0*b0     z2 = a1*b1     z1 = (a0+a1)*(b0+b1) - z0 - z2
  e il risultato e' z2*B^2m + z1*B^m + z0. Da O(n^2) a O(n^1.585).
  ⛔ LA SOGLIA NON E' UN DETTAGLIO: sotto, le tre chiamate e le somme costano piu' dei
  quattro prodotti che evitano. Il valore qui e' MISURATO su questa macchina.
  ⚠️ Lo spazio di lavoro e' UNO, passato giu' per la ricorsione: allocare a ogni livello
  rifarebbe l'errore che questo cantiere ha gia' pagato tre volte. }
const
  KARATSUBA_MIN = 24;
  { ⭐⭐⭐ TOOM-3. Spezzando in TRE parti servono CINQUE prodotti di un terzo di taglia
    invece dei nove scolastici: O(n^1.465), meglio dell'1.585 di Karatsuba. Il conto a
    parita' di n: Karatsuba fa 3*(n/2)^2 = 0,75 n^2, Toom-3 fa 5*(n/3)^2 = 0,56 n^2.
    ⛔ IL PREZZO E' L'INTERPOLAZIONE, ed e' li' che si sbaglia: cinque punti di
    valutazione (0, 1, -1, 2, infinito), divisioni ESATTE per 2 e per 3, e un valore
    che puo' essere NEGATIVO. La soglia e' alta proprio perche' quel contorno costa.

    ⛔⛔ LA SOGLIA E' MISURATA, E IL NUMERO E' MOLTO PIU' ALTO DI QUELLO DI KARATSUBA:
    il costo LINEARE di Toom-3 (valutazione + interpolazione, ~12 passate sull'intero
    valore per livello) e' quello che decide, non l'esponente. Misurato il 14 ago 2026
    su questa macchina, prodotto di due numeri di n limb, migliore di 15 corse x 3
    passate contro un binario identico salvo questa costante:

      limb   320    400    512    700   1000   1400   2000
      soglia 350   -0,1%  -9,6%  -7,5%  -1,4% -10,1% -10,2%  -9,1%
      soglia 200   +2,3% -10,9%  -7,7%  -4,8%  -4,0% -11,4%  -3,4%

    ⭐ Abbassare la soglia NON e' meglio: a 2000 limb la soglia 200 rende -3,4% dove la
    350 rende -9,1%, perche' ogni livello di Toom in piu' paga il suo costo lineare su
    sotto-prodotti troppo piccoli per ripagarlo. Con una soglia di 130 il prodotto a 160
    limb era +14,7%: una PERDITA, riprodotta due volte.
    ⚠️ Il pavimento di rumore di questa misura e' ~4%: i valori sotto i 400 limb non
    dicono niente, ed e' per questo che la soglia sta dove il divario e' stabile. }
  TOOM3_MIN = 350;
  { 3 * INV3 = 1 (mod 2^64): la divisione esatta per 3 e' una MOLTIPLICAZIONE. }
  INV3 = QWord($AAAAAAAAAAAAAAAB);

procedure PropAdd(p: PQWord; c: QWord);
var t: QWord;
begin
  while c <> 0 do
  begin
    t := p^ + c;
    if t < c then c := 1 else c := 0;
    p^ := t; Inc(p);
  end;
end;

procedure PropSub(p: PQWord; c: QWord);
var t: QWord; old: QWord;
begin
  while c <> 0 do
  begin
    old := p^;
    t := old - c;
    if old < c then c := 1 else c := 0;
    p^ := t; Inc(p);
  end;
end;

{ ⛔ Le due sopra camminano finche' il riporto non si spegne, il che va bene quando si sa
  che il numero non trabocca. L'interpolazione di Toom lavora in COMPLEMENTO A DUE su una
  larghezza fissa, dove il riporto in uscita si BUTTA: senza un limite, su un valore
  negativo (tutti $FF..F) la propagazione uscirebbe dal vettore. Un limite in piu' costa
  un confronto; la corruzione silenziosa costa una giornata. }
procedure PropAddLim(p: PQWord; c: QWord; n: PtrInt);
var t: QWord;
begin
  while (c <> 0) and (n > 0) do
  begin
    t := p^ + c;
    if t < c then c := 1 else c := 0;
    p^ := t; Inc(p); Dec(n);
  end;
end;

procedure PropSubLim(p: PQWord; c: QWord; n: PtrInt);
var t, old: QWord;
begin
  while (c <> 0) and (n > 0) do
  begin
    old := p^;
    t := old - c;
    if old < c then c := 1 else c := 0;
    p^ := t; Inc(p); Dec(n);
  end;
end;

{ d = x + y e d = x - y su n limb, col riporto/prestito in uscita. Esistono per non
  ripetere l'{$IFDEF} dentro il corpo di Toom, che e' gia' abbastanza da leggere.
  ⚠️ d PUO' coincidere con x o con y: per ogni indice si legge prima e si scrive dopo. }
function RunAdd(d, x, y: PQWord; n: PtrInt): QWord;
{$IFNDEF CPUX86_64}
var i: PtrInt; s: QWord;
{$ENDIF}
begin
  {$IFDEF CPUX86_64}
  Result := AddLimbRun(d, x, y, n);
  {$ELSE}
  Result := 0;
  for i := 0 to n - 1 do
  begin
    s := x[i] + y[i];
    if s < x[i] then
    begin
      d[i] := s + Result; Result := 1;
    end
    else
    begin
      d[i] := s + Result;
      if d[i] < s then Result := 1 else Result := 0;
    end;
  end;
  {$ENDIF}
end;

function RunSub(d, x, y: PQWord; n: PtrInt): QWord;
{$IFNDEF CPUX86_64}
var i: PtrInt; s, xi: QWord;
{$ENDIF}
begin
  {$IFDEF CPUX86_64}
  Result := SubLimbRun(d, x, y, n);
  {$ELSE}
  Result := 0;
  for i := 0 to n - 1 do
  begin
    xi := x[i];
    s := xi - y[i];
    if xi < y[i] then
    begin
      d[i] := s - Result; Result := 1;
    end
    else
    begin
      d[i] := s - Result;
      if s < Result then Result := 1 else Result := 0;
    end;
  end;
  {$ENDIF}
end;

{ ---- l'aritmetica in COMPLEMENTO A DUE su larghezza fissa, per l'interpolazione ----
  ⭐⭐⭐ E' LA DECISIONE CHE TOGLIE DI MEZZO I SEGNI. L'interpolazione di Toom-3 passa per
  valori negativi, e la via ovvia - portarsi dietro modulo e segno a ogni passo - e'
  esattamente il posto dove tutti sbagliano. Qui invece ogni valore intermedio vive in
  complemento a due su L limb, con L scelto perche' |valore| < B^L/2: somma e sottrazione
  sono quelle senza segno, il riporto in uscita si butta, e il segno esiste solo come bit
  alto. Le divisioni ESATTE (per 2 e per 3) sopravvivono al modulo: il quoziente vero e'
  congruo a quello calcolato, e sta nell'intervallo, quindi e' quello.
  ⛔ L'UNICO segno che resta e' quello di W(-1), che non si puo' evitare perche' il
  PRODOTTO vuole due magnitudini. E' un booleano, non un'algebra. }

{ d[0..L-1] := s[0..sn-1], esteso con zeri (il valore e' non negativo). }
procedure TcSet(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var i: PtrInt;
begin
  if sn > L then sn := L;
  for i := 0 to sn - 1 do d[i] := s[i];
  for i := sn to L - 1 do d[i] := 0;
end;

{ d += s (sn limb, esteso con zeri), modulo B^L. }
procedure TcAddN(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var c: QWord;
begin
  if sn > L then sn := L;
  c := RunAdd(d, d, s, sn);
  PropAddLim(@d[sn], c, L - sn);
end;

{ d -= s (sn limb, esteso con zeri), modulo B^L. }
procedure TcSubN(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var c: QWord;
begin
  if sn > L then sn := L;
  c := RunSub(d, d, s, sn);
  PropSubLim(@d[sn], c, L - sn);
end;

{ ⭐⭐ d -= (t shl s), in UNA passata. La forma ovvia - prima lo spostamento in un
  vettore d'appoggio, poi la sottrazione - sono DUE passate sull'intero valore piu' un
  vettore in piu'. A questa taglia il costo lineare non e' contorno: e' misurato che si
  mangiava due terzi del guadagno di Toom-3. }
procedure TcSubShl(d, t: PQWord; L: PtrInt; s: Integer);
var i: PtrInt; carry, cur, v, old: QWord; bo: QWord;
begin
  carry := 0; bo := 0;
  for i := 0 to L - 1 do
  begin
    cur := t[i];
    v := (cur shl s) or carry;
    carry := cur shr (64 - s);
    old := d[i];
    d[i] := old - v - bo;
    { il prestito: o non bastava per v, o v era gia' tutto e il prestito lo sfonda }
    if (old < v) or ((old = v) and (bo <> 0)) then bo := 1 else bo := 0;
  end;
end;

{ ⭐⭐ d := (d shl 1) + s, in UNA passata: e' il passo di Horner della valutazione in 2
  (A(2) = ((a2*2) + a1)*2 + a0), e per la stessa ragione non si fa in due. }
procedure TcShl1AddN(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var i: PtrInt; carry, cur, v, t: QWord; c2: QWord;
begin
  if sn > L then sn := L;
  carry := 0; c2 := 0;
  for i := 0 to L - 1 do
  begin
    cur := d[i];
    v := (cur shl 1) or carry;
    carry := cur shr 63;
    if i < sn then t := s[i] else t := 0;
    v := v + t;
    if v < t then
    begin
      d[i] := v + c2; c2 := 1;
    end
    else
    begin
      d[i] := v + c2;
      if d[i] < v then c2 := 1 else c2 := 0;
    end;
  end;
end;

{ d := d div 2, ARITMETICO: il bit alto si ricopia, altrimenti un valore negativo
  diventerebbe enorme e positivo. La divisione e' esatta per costruzione. }
procedure TcShr1(d: PQWord; L: PtrInt);
var i: PtrInt; sgn: QWord;
begin
  sgn := d[L - 1] and QWord($8000000000000000);
  for i := 0 to L - 2 do
    d[i] := (d[i] shr 1) or (d[i + 1] shl 63);
  d[L - 1] := (d[L - 1] shr 1) or sgn;
end;

{ d := d div 3, ESATTA. ⭐ Non e' una divisione: si moltiplica per l'inverso di 3 modulo
  2^64, limb per limb, portandosi dietro quanto e' "avanzato" - che vale al piu' 2.
  Funziona anche sui valori negativi, perche' il modulo non distingue. }
procedure TcDivExact3(d: PQWord; L: PtrInt);
var i: PtrInt; x, q, bo, c: QWord;
begin
  c := 0;
  for i := 0 to L - 1 do
  begin
    x := d[i];
    if x < c then bo := 1 else bo := 0;
    x := x - c;
    q := x * INV3;
    d[i] := q;
    c := MulHi64(q, 3) + bo;
  end;
end;

{ confronto di due magnitudini di pari lunghezza, dal limb piu' alto }
function CmpRun(x, y: PQWord; n: PtrInt): Integer;
var i: PtrInt;
begin
  for i := n - 1 downto 0 do
  begin
    if x[i] > y[i] then Exit(1);
    if x[i] < y[i] then Exit(-1);
  end;
  Result := 0;
end;

{ d[0..an+bn-1] = a*b, schoolbook, con l'addmul in assembly come ciclo interno. }
procedure MulSchoolbook(d, a, b: PQWord; an, bn: PtrInt);
var
  i: PtrInt;
  carry: QWord;
begin
  for i := 0 to an + bn - 1 do d[i] := 0;
  for i := 0 to an - 1 do
  begin
    if a[i] = 0 then Continue;
    {$IFDEF CPUX86_64}
    carry := AddMulLimbRun(@d[i], b, bn, a[i]);
    {$ELSE}
    carry := AddMulSlow(@d[i], b, bn, a[i]);
    {$ENDIF}
    PropAdd(@d[i + bn], carry);
  end;
end;

{$IFNDEF CPUX86_64}
function AddMulSlow(d, a: PQWord; n: PtrInt; k: QWord): QWord;
var i: PtrInt; lo, hi, t: QWord;
begin
  Result := 0;
  for i := 0 to n - 1 do
  begin
    lo := a[i] * k; hi := MulHi64Portable(a[i], k);
    t := lo + Result; if t < lo then Inc(hi);
    d[i] := d[i] + t; if d[i] < t then Inc(hi);
    Result := hi;
  end;
end;
{$ENDIF}

procedure MulRec(d, a, b, ws: PQWord; n: PtrInt); forward;

{ ⭐⭐⭐ TOOM-3, d[0..2n-1] = a[0..n-1] * b[0..n-1].

  Si spezzano i due fattori in TRE parti di k limb (l'ultima ne ha n2 <= k):
      a = a0 + a1*B^k + a2*B^2k          b = b0 + b1*B^k + b2*B^2k
  Il prodotto e' un polinomio di grado 4 in B^k:
      P(x) = c0 + c1 x + c2 x^2 + c3 x^3 + c4 x^4
  di cui bastano CINQUE valori per ricostruire i coefficienti. I punti sono 0, 1, -1, 2 e
  "infinito" (cioe' il coefficiente di testa):
      W0 = a0*b0                W4 = a2*b2
      W1 = A(1)*B(1)            Wm = A(-1)*B(-1)            W2 = A(2)*B(2)

  L'INTERPOLAZIONE, che e' la parte che si sbaglia, in sei passi:
      tA = (W1 + Wm)/2 = c0 + c2 + c4        tB = (W1 - Wm)/2 = c1 + c3
      c2 = tA - c0 - c4
      tC = (W2 - c0 - 16 c4 - 4 c2)/2 = c1 + 4 c3
      c3 = (tC - tB)/3                       c1 = tB - c3
  ⛔ Le divisioni sono ESATTE - ogni numeratore e' divisibile per costruzione - e vanno
  fatte come tali: una divisione lunga qui costerebbe piu' del prodotto che si evita.
  ⛔ tB, tC e i loro addendi possono essere NEGATIVI: vivono in complemento a due su L
  limb (vedi le TcXxx sopra), e L e' scelto perche' nessuno arrivi mai a B^L/2.

  ⚠️ I cinque prodotti si fanno tutti a taglia k+1, anche W0 e W4 che ne userebbero meno:
  la ricorsione e' scritta per operandi di PARI taglia, ed e' cio' che tiene verificabile
  la contabilita' degli indici. Il poco che si spreca si rivede quando una misura dira'
  che vale la pena. }
procedure MulToom3(d, a, b, ws: PQWord; n: PtrInt);
var
  k, n2, L, i: PtrInt;
  W0, W1, WM, W2, W4, TA, TB, TC, TT, EA, EB, rest: PQWord;
  sa, sb, sm: Integer;
  c: QWord;

  { ⭐ La valutazione in -1 e' l'UNICO posto dove nasce un segno: a0 - a1 + a2 puo'
    essere negativo, e il prodotto vuole una magnitudine. Si calcola |.| e si riporta
    il segno; il resto dell'interpolazione non ne sa niente. }
  function EvalMinus1(dst, tmp, src: PQWord): Integer;
  begin
    TcSet(dst, k + 1, src, k);                 { a0 }
    TcAddN(dst, k + 1, @src[2 * k], n2);       { + a2 }
    TcSet(tmp, k + 1, @src[k], k);             { a1 }
    if CmpRun(dst, tmp, k + 1) >= 0 then
    begin
      RunSub(dst, dst, tmp, k + 1);            { a0+a2 >= a1 }
      Result := 0;
    end
    else
    begin
      RunSub(dst, tmp, dst, k + 1);            { il verso opposto, e il segno }
      Result := 1;
    end;
  end;

begin
  k := (n + 2) div 3;          { ceil(n/3): le due parti basse }
  n2 := n - 2 * k;             { la parte alta, 1 <= n2 <= k }
  L := 2 * k + 4;              { larghezza dei valori intermedi, con due limb di aria }

  W0 := ws;            W1 := @ws[L];       WM := @ws[2 * L];
  W2 := @ws[3 * L];    W4 := @ws[4 * L];
  TA := @ws[5 * L];    TB := @ws[6 * L];   TC := @ws[7 * L];  TT := @ws[8 * L];
  EA := @ws[9 * L];    EB := @ws[9 * L + (k + 2)];
  rest := @ws[9 * L + 2 * (k + 2)];

  { --- W0 = a0*b0, e W4 = a2*b2 --- }
  TcSet(EA, k + 1, a, k);          TcSet(EB, k + 1, b, k);
  MulRec(W0, EA, EB, rest, k + 1); W0[2 * k + 2] := 0; W0[2 * k + 3] := 0;

  TcSet(EA, k + 1, @a[2 * k], n2); TcSet(EB, k + 1, @b[2 * k], n2);
  MulRec(W4, EA, EB, rest, k + 1); W4[2 * k + 2] := 0; W4[2 * k + 3] := 0;

  { --- W1 = A(1)*B(1), con A(1) = a0+a1+a2 < 3*B^k, che sta in k+1 limb --- }
  TcSet(EA, k + 1, a, k);  TcAddN(EA, k + 1, @a[k], k);  TcAddN(EA, k + 1, @a[2 * k], n2);
  TcSet(EB, k + 1, b, k);  TcAddN(EB, k + 1, @b[k], k);  TcAddN(EB, k + 1, @b[2 * k], n2);
  MulRec(W1, EA, EB, rest, k + 1); W1[2 * k + 2] := 0; W1[2 * k + 3] := 0;

  { --- Wm = |A(-1)| * |B(-1)|, col segno a parte --- }
  sa := EvalMinus1(EA, TT, a);
  sb := EvalMinus1(EB, TT, b);
  sm := sa xor sb;
  MulRec(WM, EA, EB, rest, k + 1); WM[2 * k + 2] := 0; WM[2 * k + 3] := 0;

  { --- W2 = A(2)*B(2), con A(2) = a0 + 2a1 + 4a2 < 7*B^k: Horner, due raddoppi --- }
  TcSet(EA, k + 1, @a[2 * k], n2);
  TcShl1AddN(EA, k + 1, @a[k], k); TcShl1AddN(EA, k + 1, a, k);
  TcSet(EB, k + 1, @b[2 * k], n2);
  TcShl1AddN(EB, k + 1, @b[k], k); TcShl1AddN(EB, k + 1, b, k);
  MulRec(W2, EA, EB, rest, k + 1); W2[2 * k + 2] := 0; W2[2 * k + 3] := 0;

  { ================= interpolazione ================= }
  { tA = (W1 + Wm)/2, tB = (W1 - Wm)/2 - col segno di Wm che decide il verso.
    ⭐ Si scrive DIRETTAMENTE nel destinatario: la copia di W1 e poi la somma in posto
    erano due passate dove ne basta una. }
  if sm = 0 then
  begin
    RunAdd(TA, W1, WM, L);
    RunSub(TB, W1, WM, L);
  end
  else
  begin
    RunSub(TA, W1, WM, L);
    RunAdd(TB, W1, WM, L);
  end;
  TcShr1(TA, L);                      { tA = c0 + c2 + c4 }
  TcShr1(TB, L);                      { tB = c1 + c3      }

  { c2 = tA - c0 - c4, e da qui TA E' c2 }
  TcSubN(TA, L, W0, L);
  TcSubN(TA, L, W4, L);

  { tC = (W2 - c0 - 16 c4 - 4 c2)/2 = c1 + 4 c3 }
  RunSub(TC, W2, W0, L);
  TcSubShl(TC, W4, L, 4);
  TcSubShl(TC, TA, L, 2);
  TcShr1(TC, L);

  { c3 = (tC - tB)/3, poi c1 = tB - c3 }
  TcSubN(TC, L, TB, L);
  TcDivExact3(TC, L);                 { TC e' c3 }
  TcSubN(TB, L, TC, L);               { TB e' c1 }

  { ================= si rimonta il risultato =================
    d = c0 + c1 B^k + c2 B^2k + c3 B^3k + c4 B^4k, e le lunghezze NON si tirano a
    indovinare: c1 e c2 valgono meno di B^(2k+1), c3 meno di B^(k+n2+1), c4 meno di
    B^(2*n2). ⛔ Ognuna di queste finisce dentro i 2n limb di d - il conto e' nel
    commento della procedura - e la propagazione del riporto e' LIMITATA a quel che
    resta: un riporto che uscisse da d sarebbe memoria di qualcun altro. }
  { ⭐ c0 e c4 si SCRIVONO, non si sommano: occupano esattamente [0,2k) e [4k,2n), che
    sono ancora vergini. Restano da azzerare i soli 2k limb di mezzo. }
  for i := 0 to 2 * k - 1 do d[i] := W0[i];
  for i := 0 to 2 * n2 - 1 do d[4 * k + i] := W4[i];
  for i := 2 * k to 4 * k - 1 do d[i] := 0;

  c := RunAdd(@d[k], @d[k], TB, 2 * k + 1);
  PropAddLim(@d[3 * k + 1], c, 2 * n - 3 * k - 1);

  c := RunAdd(@d[2 * k], @d[2 * k], TA, 2 * k + 1);
  PropAddLim(@d[4 * k + 1], c, 2 * n - 4 * k - 1);

  c := RunAdd(@d[3 * k], @d[3 * k], TC, k + n2 + 1);
  PropAddLim(@d[4 * k + n2 + 1], c, 2 * n - 4 * k - n2 - 1);
end;

{ Lo spazio di lavoro che MulRec richiede per n limb, CALCOLATO invece che stimato.
  ⛔ Una stima generosa e' comunque una stima: qui sotto ci sono cinque puntatori
  ricavati da k e un livello di ricorsione, e sbagliarla di un limb non da' un errore -
  da' un risultato giusto quasi sempre. }
function MulWsNeed(n: PtrInt): PtrInt;
var k, h: PtrInt;
begin
  if n < KARATSUBA_MIN then Exit(0);
  if n >= TOOM3_MIN then
  begin
    k := (n + 2) div 3;
    Exit(9 * (2 * k + 4) + 2 * (k + 2) + MulWsNeed(k + 1));
  end;
  h := n - n div 2;
  Exit(4 * (h + 1) + MulWsNeed(h + 1));
end;

{ d[0..2n-1] = a[0..n-1] * b[0..n-1]. ws: spazio di lavoro di MulWsNeed(n) limb. }
procedure MulRec(d, a, b, ws: PQWord; n: PtrInt);
var
  m, h, i: PtrInt;
  t0, t1, z1: PQWord;
  c: QWord;
begin
  if n < KARATSUBA_MIN then
  begin
    MulSchoolbook(d, a, b, n, n);
    Exit;
  end;
  if n >= TOOM3_MIN then
  begin
    MulToom3(d, a, b, ws, n);
    Exit;
  end;
  m := n div 2;          { parte bassa }
  h := n - m;            { parte alta, h >= m }

  MulRec(d, a, b, ws, m);                        { z0 = a0*b0  -> d[0 .. 2m-1] }
  MulRec(@d[2 * m], @a[m], @b[m], ws, h);        { z2 = a1*b1  -> d[2m .. 2m+2h-1] }

  { t0 = a0 + a1 e t1 = b0 + b1, h+1 limb ciascuno }
  t0 := ws;
  t1 := @ws[h + 1];
  z1 := @ws[2 * (h + 1)];
  for i := 0 to h - 1 do t0[i] := a[m + i];
  t0[h] := 0;
  {$IFDEF CPUX86_64}
  c := AddLimbRun(t0, t0, a, m);
  {$ELSE}
  c := 0;
  for i := 0 to m - 1 do
  begin
    t0[i] := t0[i] + c; if t0[i] < c then c := 1 else c := 0;
    t0[i] := t0[i] + a[i]; if t0[i] < a[i] then c := 1;
  end;
  {$ENDIF}
  PropAdd(@t0[m], c);

  for i := 0 to h - 1 do t1[i] := b[m + i];
  t1[h] := 0;
  {$IFDEF CPUX86_64}
  c := AddLimbRun(t1, t1, b, m);
  {$ELSE}
  c := 0;
  for i := 0 to m - 1 do
  begin
    t1[i] := t1[i] + c; if t1[i] < c then c := 1 else c := 0;
    t1[i] := t1[i] + b[i]; if t1[i] < b[i] then c := 1;
  end;
  {$ENDIF}
  PropAdd(@t1[m], c);

  { z1 = t0 * t1, 2(h+1) limb, con spazio di lavoro OLTRE quello gia' occupato }
  MulRec(z1, t0, t1, @ws[4 * (h + 1)], h + 1);

  { z1 -= z0 (2m limb) e z1 -= z2 (2h limb) }
  {$IFDEF CPUX86_64}
  c := SubLimbRun(z1, z1, d, 2 * m);
  {$ELSE}
  c := 0;
  for i := 0 to 2 * m - 1 do
  begin
    if z1[i] < d[i] then begin z1[i] := z1[i] - d[i] - c; c := 1; end
    else begin if z1[i] - d[i] < c then begin z1[i] := z1[i] - d[i] - c; c := 1; end
               else begin z1[i] := z1[i] - d[i] - c; c := 0; end; end;
  end;
  {$ENDIF}
  PropSub(@z1[2 * m], c);
  {$IFDEF CPUX86_64}
  c := SubLimbRun(z1, z1, @d[2 * m], 2 * h);
  {$ELSE}
  c := 0;
  for i := 0 to 2 * h - 1 do
  begin
    if z1[i] < d[2*m + i] then begin z1[i] := z1[i] - d[2*m + i] - c; c := 1; end
    else begin if z1[i] - d[2*m + i] < c then begin z1[i] := z1[i] - d[2*m + i] - c; c := 1; end
               else begin z1[i] := z1[i] - d[2*m + i] - c; c := 0; end; end;
  end;
  {$ENDIF}
  PropSub(@z1[2 * h], c);

  { d[m ..] += z1, che occupa al piu' 2h+2 limb }
  {$IFDEF CPUX86_64}
  c := AddLimbRun(@d[m], @d[m], z1, 2 * h + 2);
  {$ELSE}
  c := 0;
  for i := 0 to 2 * h + 1 do
  begin
    d[m + i] := d[m + i] + c; if d[m + i] < c then c := 1 else c := 0;
    d[m + i] := d[m + i] + z1[i]; if d[m + i] < z1[i] then c := 1;
  end;
  {$ENDIF}
  PropAdd(@d[m + 2 * h + 2], c);
end;

procedure BigMul(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
var
  t: TLimbs;
  ka, kb, kws: TLimbs;
  kn: Integer;
  i, j, k: Integer;
  lo, hi, s, carry: QWord;
begin
  if (an <= 0) or (bn <= 0) then
  begin
    UniqueLimbs(dst);
    if Length(dst) < 1 then SetLength(dst, 1);
    dst[0] := 0; dn := 1;
    Exit;
  end;
  {$IFDEF CPUX86_64}
  { ⭐ Sopra la soglia, e con entrambi i fattori abbastanza grandi, si passa a Karatsuba.
    I due fattori vengono portati alla STESSA lunghezza con zeri in testa: la ricorsione
    e' scritta per operandi di pari taglia, ed e' cio' che tiene semplice - e quindi
    verificabile - la contabilita' degli indici. }
  if (an >= KARATSUBA_MIN) and (bn >= KARATSUBA_MIN) then
  begin
    kn := an; if bn > kn then kn := bn;
    SetLength(ka, kn); SetLength(kb, kn);
    for i := 0 to kn - 1 do
    begin
      if i < an then ka[i] := a[i] else ka[i] := 0;
      if i < bn then kb[i] := b[i] else kb[i] := 0;
    end;
    SetLength(t, 2 * kn);
    SetLength(kws, MulWsNeed(kn) + 8);
    MulRec(@t[0], @ka[0], @kb[0], @kws[0], kn);
    dn := 2 * kn;
    while (dn > 1) and (t[dn - 1] = 0) do Dec(dn);
    dst := t;
    Exit;
  end;
  {$ENDIF}
  SetLength(t, an + bn);                 { azzerato da SetLength su un array nuovo }
  for i := 0 to an - 1 do
  begin
    if a[i] = 0 then Continue;           { una cifra nulla non contribuisce: saltarla }
    {$IFDEF CPUX86_64}
    { ⭐ Il ciclo interno del prodotto scolastico E' mpn_addmul_1. }
    carry := AddMulLimbRun(@t[i], @b[0], bn, a[i]);
    {$ELSE}
    carry := 0;
    for j := 0 to bn - 1 do
    begin
      lo := a[i] * b[j];
      hi := MulHi64(a[i], b[j]);
      { Tre addendi sul limb i+j: quello che c'e' gia', la parte bassa del prodotto e
        il riporto. ⛔ OGNI somma puo' traboccare, e ogni trabocco va nella parte ALTA:
        dimenticarne uno sbaglia solo su certi valori, che e' il modo peggiore. }
      s := t[i + j] + lo;  if s < lo then Inc(hi);
      s := s + carry;      if s < carry then Inc(hi);
      t[i + j] := s;
      carry := hi;
    end;
    {$ENDIF}
    k := i + bn;
    while carry <> 0 do
    begin
      s := t[k] + carry;
      if s < carry then carry := 1 else carry := 0;
      t[k] := s;
      Inc(k);
    end;
  end;
  dn := an + bn;
  while (dn > 1) and (t[dn - 1] = 0) do Dec(dn);
  dst := t;                              { consegna: nessun aliasing possibile }
end;

procedure BigAdd(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
{ ⛔⛔⛔ COSTRUISCE IN UN VETTORE A PARTE, e non e' pignoleria. `x = x + y` fa arrivare
  qui dst e a come LO STESSO array: il vecchio codice faceva `SetLength(dst, m+1)`, che
  RIALLOCA, e il parametro `const a` restava a puntare al blocco appena LIBERATO. Da
  quel momento si legge memoria morta e si corrompe l'heap - il sintomo era un SIGSEGV
  dentro SysFreeMem di FPC, molto piu' tardi e su un'operazione innocente, con tutti i
  valori stampati fino a quel punto CORRETTI.
  ⚠️ Si vede solo quando la somma FA CRESCERE dst: con un numero che sta gia' nei limb
  disponibili il SetLength non rialloca e tutto sembra funzionare. 14 ago 2026. }
var
  t: TLimbs;
  i, m: Integer;
  s, carry, x: QWord;
begin
  m := an; if bn > m then m := bn;
  { ⭐ IL TEMPORANEO SOLO QUANDO SERVE. Il pericolo e' la RIALLOCAZIONE mentre `a` o `b`
    puntano allo stesso blocco di dst; se dst e' gia' abbastanza capiente non si
    rialloca nulla e si puo' scrivere in posto, che e' il caso NORMALE dentro un ciclo
    (dopo i primi giri il vettore ha gia' la sua taglia). Misurato: e' la differenza
    fra un'allocazione per operazione e nessuna. }
  if (Length(dst) >= m + 1) and (LimbsRefCount(dst) <= 1) then
  begin
    {$IFDEF CPUX86_64}
    { ⭐ Lunghezze uguali: e' esattamente una catena ADC, senza rami. E' il caso normale
      dell'accumulo, dove i due operandi sono cresciuti insieme. }
    if (an = bn) and (an > 0) then
    begin
      carry := AddLimbRun(@dst[0], @a[0], @b[0], an);
      dn := an;
      if carry > 0 then begin dst[dn] := carry; Inc(dn); end;
      while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
      Exit;
    end;
    {$ENDIF}
    carry := 0;
    for i := 0 to m - 1 do
    begin
      s := carry; carry := 0;
      if i < an then begin x := s + a[i]; if x < s then carry := 1; s := x; end;
      if i < bn then begin x := s + b[i]; if x < s then Inc(carry); s := x; end;
      dst[i] := s;      { stesso indice letto e scritto: l'aliasing e' innocuo }
    end;
    dn := m;
    if carry > 0 then begin dst[dn] := carry; Inc(dn); end;
    while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
    Exit;
  end;
  SetLength(t, m + 1);
  carry := 0;
  for i := 0 to m - 1 do
  begin
    s := carry; carry := 0;
    if i < an then begin x := s + a[i]; if x < s then carry := 1; s := x; end;
    if i < bn then begin x := s + b[i]; if x < s then Inc(carry); s := x; end;
    t[i] := s;
  end;
  dn := m;
  if carry > 0 then begin t[dn] := carry; Inc(dn); end;
  while (dn > 1) and (t[dn - 1] = 0) do Dec(dn);
  dst := t;
end;

procedure BigSub(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
var
  t: TLimbs;
  i: Integer;
  bb, v, borrow: QWord;
begin
  { Stesso motivo di BigAdd, e stessa scorciatoia: in posto quando non si rialloca. }
  if (Length(dst) >= an) and (LimbsRefCount(dst) <= 1) then
  begin
    {$IFDEF CPUX86_64}
    if (an = bn) and (an > 0) then
    begin
      SubLimbRun(@dst[0], @a[0], @b[0], an);   { il chiamante garantisce a >= b: niente prestito finale }
      dn := an;
      while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
      Exit;
    end;
    {$ENDIF}
    borrow := 0;
    for i := 0 to an - 1 do
    begin
      if i < bn then bb := b[i] else bb := 0;
      v := a[i] - bb;
      if (a[i] < bb) or (v < borrow) then
      begin dst[i] := v - borrow; borrow := 1; end
      else
      begin dst[i] := v - borrow; borrow := 0; end;
    end;
    dn := an;
    while (dn > 1) and (dst[dn - 1] = 0) do Dec(dn);
    Exit;
  end;
  SetLength(t, an);
  borrow := 0;
  for i := 0 to an - 1 do
  begin
    if i < bn then bb := b[i] else bb := 0;
    v := a[i] - bb;
    if a[i] < bb then
    begin
      t[i] := v - borrow;
      borrow := 1;
    end
    else if v < borrow then
    begin
      t[i] := v - borrow;
      borrow := 1;
    end
    else
    begin
      t[i] := v - borrow;
      borrow := 0;
    end;
  end;
  dn := an;
  while (dn > 1) and (t[dn - 1] = 0) do Dec(dn);
  dst := t;
end;

function LimbClz(x: QWord): Integer;
begin
  Result := 0;
  if x = 0 then Exit(64);
  while (x and QWord($8000000000000000)) = 0 do begin x := x shl 1; Inc(Result); end;
end;

procedure BigDivMod(var q: TLimbs; var qn: Integer; var r: TLimbs; var rn: Integer;
                    const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer;
                    var wu, wv: TLimbs);
var
  u, v, qq: TLimbs;
  s, i, j, m, n: Integer;
  qhat, rhat, carry, borrow, t, lo, hi, p: QWord;
  neg: Boolean;
begin
  { normalizza le lunghezze in ingresso }
  while (an > 1) and (a[an - 1] = 0) do Dec(an);
  while (bn > 1) and (b[bn - 1] = 0) do Dec(bn);

  if (bn = 1) and (b[0] = 0) then begin qn := 1; rn := 1; SetLength(q,1); SetLength(r,1); q[0]:=0; r[0]:=0; Exit; end;

  if (an < bn) or ((an = bn) and (BigCmp(a, an, b, bn) < 0)) then
  begin
    { a < b: quoziente 0, resto a }
    UniqueLimbs(q); if Length(q) < 1 then SetLength(q, 1);
    q[0] := 0; qn := 1;
    UniqueLimbs(r); if Length(r) < an then SetLength(r, an);
    for i := 0 to an - 1 do r[i] := a[i];
    rn := an;
    Exit;
  end;

  if bn = 1 then
  begin
    { divisore di un solo limb: una passata con DIVQ, niente algoritmo D }
    if Length(wu) < an then SetLength(wu, an);
    rhat := 0;
    for i := an - 1 downto 0 do
      wu[i] := DivMod128By64(rhat, a[i], b[0], rhat);
    qn := an; while (qn > 1) and (wu[qn - 1] = 0) do Dec(qn);
    UniqueLimbs(q); if Length(q) < qn then SetLength(q, qn);
    for i := 0 to qn - 1 do q[i] := wu[i];
    UniqueLimbs(r); if Length(r) < 1 then SetLength(r, 1);
    r[0] := rhat; rn := 1;
    Exit;
  end;

  n := bn; m := an - bn;
  s := LimbClz(b[n - 1]);

  { v = b << s, nello spazio di lavoro }
  if Length(wv) < n then SetLength(wv, n);
  v := wv;
  if s = 0 then
    for i := 0 to n - 1 do v[i] := b[i]
  else
  begin
    for i := n - 1 downto 1 do v[i] := (b[i] shl s) or (b[i - 1] shr (64 - s));
    v[0] := b[0] shl s;
  end;

  { u = a << s, con un limb in piu' in cima, nello spazio di lavoro }
  if Length(wu) < an + 1 then SetLength(wu, an + 1);
  u := wu;
  if s = 0 then
  begin
    for i := 0 to an - 1 do u[i] := a[i];
    u[an] := 0;
  end
  else
  begin
    u[an] := a[an - 1] shr (64 - s);
    for i := an - 1 downto 1 do u[i] := (a[i] shl s) or (a[i - 1] shr (64 - s));
    u[0] := a[0] shl s;
  end;

  UniqueLimbs(q); if Length(q) < m + 1 then SetLength(q, m + 1);
  qq := q;
  for j := m downto 0 do
  begin
    { stima da due limb su uno; u[j+n] < v[n-1] e' l'invariante che regge DIVQ }
    if u[j + n] >= v[n - 1] then
    begin
      qhat := QWord($FFFFFFFFFFFFFFFF);
      rhat := u[j + n - 1] + v[n - 1];
      neg := rhat < v[n - 1];      { il riporto dice che rhat e' gia' oltre un limb }
    end
    else
    begin
      qhat := DivMod128By64(u[j + n], u[j + n - 1], v[n - 1], rhat);
      neg := False;
    end;
    { correzione: finche' qhat*v[n-2] > rhat:u[j+n-2], qhat e' alto }
    while (not neg) do
    begin
      hi := MulHi64(qhat, v[n - 2]);
      lo := qhat * v[n - 2];
      if (hi > rhat) or ((hi = rhat) and (lo > u[j + n - 2])) then
      begin
        Dec(qhat);
        rhat := rhat + v[n - 1];
        if rhat < v[n - 1] then neg := True;   { rhat e' uscito dal limb: basta correggere }
      end
      else Break;
    end;

    { moltiplica e sottrai: u[j..j+n] -= qhat * v }
    {$IFDEF CPUX86_64}
    { ⭐ E' mpn_submul_1: una sola passata, il prestito ricostruito nei registri. }
    carry := SubMulLimbRun(@u[j], @v[0], n, qhat);
    borrow := 0;
    t := carry;
    {$ELSE}
    borrow := 0; carry := 0;
    for i := 0 to n - 1 do
    begin
      p := qhat * v[i];
      hi := MulHi64(qhat, v[i]);
      t := p + carry; if t < p then Inc(hi);
      carry := hi;
      if u[j + i] < t then
      begin
        u[j + i] := u[j + i] - t - borrow;
        borrow := 1;
      end
      else
      begin
        lo := u[j + i] - t;
        if lo < borrow then begin u[j + i] := lo - borrow; borrow := 1; end
        else begin u[j + i] := lo - borrow; borrow := 0; end;
      end;
    end;
    t := carry + borrow;
    {$ENDIF}
    if u[j + n] < t then begin u[j + n] := u[j + n] - t; borrow := 1; end
    else begin u[j + n] := u[j + n] - t; borrow := 0; end;

    if borrow <> 0 then
    begin
      { la stima era alta di uno: si somma indietro il divisore. Raro - circa due volte
        su 2^64 - ma senza questo ramo la divisione sbaglia proprio dove nessuno guarda. }
      Dec(qhat);
      carry := 0;
      for i := 0 to n - 1 do
      begin
        t := u[j + i] + v[i] + carry;
        if (t < u[j + i]) or ((carry = 1) and (t = u[j + i])) then carry := 1 else carry := 0;
        u[j + i] := t;
      end;
      u[j + n] := u[j + n] + carry;
    end;
    qq[j] := qhat;
  end;

  qn := m + 1; while (qn > 1) and (qq[qn - 1] = 0) do Dec(qn);

  { il resto e' u[0..n-1] >> s }
  UniqueLimbs(r); if Length(r) < n then SetLength(r, n);
  if s = 0 then
    for i := 0 to n - 1 do r[i] := u[i]
  else
  begin
    for i := 0 to n - 2 do r[i] := (u[i] shr s) or (u[i + 1] shl (64 - s));
    r[n - 1] := u[n - 1] shr s;
  end;
  rn := n; while (rn > 1) and (r[rn - 1] = 0) do Dec(rn);
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
