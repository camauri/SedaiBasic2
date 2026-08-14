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

procedure BigMul(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
var
  t: TLimbs;
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
  SetLength(t, an + bn);                 { azzerato da SetLength su un array nuovo }
  for i := 0 to an - 1 do
  begin
    if a[i] = 0 then Continue;           { una cifra nulla non contribuisce: saltarla }
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
