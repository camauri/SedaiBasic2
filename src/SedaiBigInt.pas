unit SedaiBigInt;

{ ============================================================================
  Multiple-precision arithmetic - the CORE, with no language surface.

  Plan: job/docs/PIANO_BIGINT.md. Reference for correctness and for speed:
  job/tests/bench/pid64_proto.pas (complete pidigits, MATCH with the oracle at
  1000 digits).

  ⭐ WHY IT EXISTS, in one number: pidigits with the arithmetic written in BASIC
  sits 18.9x from GMP; with the arithmetic in here the prototype measures 2.5x -
  and at N=1000 it beats GMP. The prize is 7.7-10.7x on our own AOT, and it
  splits into two halves that must BOTH be taken: 3.0x from removing the PER-LIMB
  interpretation (hence the whole loop lives here, not a per-limb primitive) and
  4.3x from base 2^64, which BASIC cannot express because a(i)*k overflows Int64.

  ⭐ THE REPRESENTATION, and why it is NOT a string. The strings of this runtime
  are FPC AnsiStrings, so they would have given refcounting and copy-on-write for
  free - and that was the first proposal. It was rejected: if a BigInt WERE a
  string, every consumer asking "what is in this string register?" (the bank, the
  spill, AOT/JIT leaf calls, PRINT, comparisons, LEN) would be handed BigInts
  without knowing it. That is the disease PIANO_TIPI.md had just cured - a record
  used to be three vectors per bank, so a byte offset did not exist as a concept.
  One thing carrying two.
  ⇒ A type of its OWN, and copy-on-write costs TEN LINES (UniqueLimbs): measured
  at 0 ms over 2000 calls when it is not shared, against the same figures as the
  string when it is.

  ⛔ INVARIANT, and it holds for every function in here: a length always describes
  a NORMALIZED magnitude - no zero limbs on top - because the comparison looks at
  the length first, and a leading zero would make the smaller number compare
  greater. Whoever shortens, normalizes.
  ============================================================================ }

{$mode objfpc}{$H+}{$asmmode att}
{$codepage UTF8}   // CP_UTF8 literals, like every other unit. A string that crosses a codepage
                   // boundary is converted and a comparison stops being a memcmp - see SedaiFileIO.

{ ⛔⛔ WRAPAROUND IS THE ALGORITHM, not an accident. The carry of an
  addition between limbs is detected PRECISELY by the overflow
  (`s := a + b; if s < b then carry`), and the low half of a product
  is the result truncated to 64 bits. With {$Q+} - which the debug
  build turns on - every one of those operations raises EIntOverflow,
  and the program dies on arithmetic that is CORRECT.
  Found 14 Aug 2026: it worked in release and not in debug, and the
  symptom arrived as an error on the wrong BASIC line.
  ⚠️ It holds for the whole unit: every function in here works on
  unsigned magnitudes and relies on modulo 2^64. }
{$Q-}{$R-}

interface

type
  { The limbs, least significant first. The length in limbs is Length(); normalization
    is an invariant, not a courtesy. }
  TLimbs = array of QWord;

{ 64x64 -> the HIGH 64 bits of the product: this platform's fast path. }
function MulHi64(a, b: QWord): QWord; {$IFDEF CPUX86_64}inline;{$ENDIF}

{ ⛔ The same thing in pure Pascal, and it is ALWAYS compiled even where it is
  not needed. It is not dead code: it is the ORACLE the fast path is checked
  against. Put in an {$ELSE} branch it would have vanished from x86-64, and the
  check comparing them would have become "assembly against itself" - green and
  blind. }
function MulHi64Portable(a, b: QWord): QWord;

{ (hi:lo) div d, with the remainder in rem. ⛔ REQUIRES hi < d, which is the invariant of
  the schoolbook step that uses it: without it the quotient would not fit in 64 bits.
  ⚠️ It is BIT BY BIT long division, 64 turns: slow on purpose. It is only needed by the
  decimal conversion, which is on no hot path (pidigits lets the digits out of the spigot
  one at a time and never converts a whole number). The x86 route would be a single `divq`,
  but that has to be written in assembly, and assembly here is paid for in calling
  conventions: it gets added when a measurement says it is needed. }
function DivMod128By64(hi, lo, d: QWord; out rem: QWord): QWord;

{$IFDEF CPUX86_64}
{ The hot limb loops in assembly. Exported because the check comparing them with the
  portable path is a separate program: a primitive nobody can verify from outside is a
  primitive nobody knows to be right. }
function MulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
function AddLimbRun(pd, pa, pb: Pointer; n: PtrInt): QWord;
function SubLimbRun(pd, pa, pb: Pointer; n: PtrInt): QWord;
function AddMulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
function SubMulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
{$ENDIF}

{ Detach ONLY if shared: the equivalent of UniqueString for a dynamic array, which on its
  own does NOT copy on write (a2 := a1; a2[0] := 9 changes a1[0] too - measured, not
  deduced). }
procedure UniqueLimbs(var a: TLimbs);
{ The refcount of a limb vector: it decides whether an in-place write is allowed. }
function LimbsRefCount(const a: TLimbs): PtrInt;

procedure BigSetSmall(var a: TLimbs; var n: Integer; v: QWord);
procedure BigCopy(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer);
{ a *= k, with k fitting in one limb. Returns the number of limbs used. }
procedure BigMulSmall(var a: TLimbs; var n: Integer; k: QWord);
{ dst = a * k in ONE pass, without copying first. ⭐ BigMulSmall works in place, so a
  destination different from the source cost a COPY plus a multiplication: two passes over
  the whole number where one is enough. It is the shape of "probe = den * q", which the
  digit loop runs up to ten times per digit.
  ⚠️ dst may alias a: it reads a[i] and writes dst[i], the same index. }
procedure BigMulSmallTo(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; k: QWord);
{ a += k, with k fitting in one limb. Paired with BigMulSmall this is all that Horner in
  base 10^19 needs, i.e. reading a decimal number. }
procedure BigAddSmall(var a: TLimbs; var n: Integer; k: QWord);
{ dst = a * b, magnitudes only. ⚠️ dst MAY be a or b without harm: the product is built in
  a separate vector and handed over at the end, because the schoolbook scheme reads a[i] and
  b[j] while writing into i+j, and with aliasing it would read digits already overwritten.
  It costs one allocation and removes a whole class of defects. }
procedure BigMul(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
procedure BigAdd(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
{ dst = a - b, magnitudes only: the caller guarantees a >= b. }
procedure BigSub(var dst: TLimbs; var dn: Integer; const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer);
{ q = a div b, r = a mod b, MAGNITUDES only. b <> 0 is the caller's responsibility.
  ⭐ It is Knuth's algorithm D (TAOCP 4.3.1): normalization, estimating the quotient from
  TWO limbs of the dividend over ONE of the divisor, correcting the estimate, multiply-and-
  subtract, and the rare "add back" when the estimate was one too high.
  ⚠️ q and r may NOT be a or b: they are built separately and handed over. }
{ ⚠️ wu and wv are the caller's WORKSPACE, not results: Knuth's normalization builds a
  shifted dividend and divisor, and allocating them on every call costs more than the
  algorithm itself on numbers of thousands of limbs (measured: the division came out SLOWER
  than the trial loop it was meant to replace). The caller keeps them and passes them back;
  they grow once and then no more. }
procedure BigDivMod(var q: TLimbs; var qn: Integer; var r: TLimbs; var rn: Integer;
                    const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer;
                    var wu, wv: TLimbs);

function BigCmp(const a: TLimbs; an: Integer; const b: TLimbs; bn: Integer): Integer;

implementation

{ ⛔ The 128-bit product is the only thing here the host language cannot express, and it is
  also the only one that reaches for assembly. Two routes:
  - x86-64: a single MUL, but ASSEMBLY MUST BE GATED PER SYSTEM. The scar is from
    12 Aug 2026: VecScanPrefix was written for the Win64 convention alone and guarded by
    {$IFDEF CPUX86_64} - the ARCHITECTURE, not the SYSTEM - and on Linux it read its
    arguments from the wrong registers, going wrong about half the time depending on ASLR.
    Here the function is written in Pascal and the compiler puts the arguments where it
    likes: no convention to guess, and no per-OS branch.
  - everywhere: four 32-bit products. Slower, but the WASM target and a possible ARM are not
    left out - and it is also the oracle the fast path is checked against. }
{$IFDEF CPUX86_64}
function MulLimbRun(pd, pa: Pointer; n: PtrInt; k: QWord): QWord;
{ dst[0..n-1] = a[0..n-1] * k, returning the carry out. ⭐ It is the hottest loop in the
  whole of multiple-precision arithmetic, and in Pascal it cost ~4 cycles per limb because
  the carry is detected with a COMPARE and a branch. Here MULQ produces rdx:rax and ADC
  takes the carry from the flags: no branch, no invented dependency.
  ⛔ The parameters are referred to BY NAME and copied straight into CALLER-saved registers
  (r8-r11, rax, rcx, rdx): touching rbx or r12-r15 without saving them would break the
  caller, and hard-wiring rdi/rsi would assume a calling convention - the mistake
  VecScanPrefix paid half a day for. Here FPC decides where they live and the assembly does
  not assume it. }
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
{ dst[0..n-1] = a[0..n-1] + b[0..n-1], returning the carry out.
  ⭐ THE ADC CHAIN lives in the carry flag, so the loop must not touch the flags between one
  limb and the next. The trick is the NEGATIVE INDEX: start at -n and climb with INC, which
  unlike ADD/SUB **does not modify CF** - it only touches ZF, which is what the branch
  needs. Without this the carry would have to be saved and restored every turn, and that is
  exactly what makes the loop written in Pascal slow. }
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
{ dst = a - b over the n common limbs; returns the BORROW out. Same shape. }
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
{ dst[0..n-1] += a[0..n-1] * k, returning the part still to be propagated. It is
  mpn_addmul_1, the inner loop of the schoolbook product.
  ⚠️ Here the carry lives in a REGISTER, not in the flags - it is rebuilt every turn with two
  ADCs - so the loop control is free to use ADD and DEC: the negative-index caution is only
  needed where the chain lives in the flag. }
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
{ dst[0..n-1] -= a[0..n-1] * k, returning the part still to be SUBTRACTED higher up.
  It is mpn_submul_1, the multiply-and-subtract of algorithm D. }
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
  { ⭐ The assembly refers to its PARAMETERS BY NAME: it is FPC that knows where it put
    them, so there is no calling convention to guess, and the {$IFDEF CPUX86_64} gate - the
    ARCHITECTURE - is the RIGHT one here, because the only thing it assumes is that the MUL
    instruction exists.
    ⛔ That is exactly what VecScanPrefix did NOT do: there the registers were hand-wired for
    Win64 under the same architecture gate, and on System V the function read its arguments
    from the wrong registers - about half the time the answer was wrong, depending on ASLR.
    See job/tests/bas/bug_regex_vecfilter.bas.
    📊 Worth 1.61x on pidigits N=3000 against the portable path. }
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
  { ⭐ A single DIVQ. As in MulHi64, the assembly refers to the names and leaves it to FPC
    to know where they are: no calling convention to guess, and the {$IFDEF CPUX86_64} gate
    is the right one because the only thing it assumes is that the instruction exists.
    ⛔ The quotient and the remainder go through LOCAL VARIABLES, not through the `out`
    parameter: for a by-reference parameter the name in assembly denotes the POINTER, not
    the place to write, and getting that wrong here would mean writing over an address.
    ⛔ DIVQ raises #DE if the quotient does not fit in 64 bits: the precondition hi < d is
    NOT a courtesy, it is what holds this function up. Algorithm D guarantees it.
    📊 It serves the inner loop of long division: the bit-by-bit route below does 64 turns
    per limb, and that is what made a real division impractical. }
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
    { ⛔ The TOP bit must be saved BEFORE the shift: rem can be as large as d-1, and with
      d above 2^63 the shift loses it. If it went out, the true value is rem + 2^64 and is
      certainly >= d, so the subtraction happens anyway - and subtracting modulo 2^64 gives
      the right result. Without this line the conversion only goes wrong on large numbers,
      which is the worst way to go wrong. }
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
    if t < lo then Inc(hi);          { the addition's carry goes into the high half }
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
    dst[i] := t;          { same index read and written: dst = a is harmless }
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
    if s < carry then carry := 1 else carry := 0;   { the overflow IS the carry }
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

{ ⭐⭐⭐ KARATSUBA. The schoolbook product is O(n^2): no quality of assembly recovers an
  order of complexity, and above a few tens of limbs that is where serious libraries win.
  Splitting a = a1*B^m + a0 and b = b1*B^m + b0 needs THREE half-size products instead of
  four:
      z0 = a0*b0     z2 = a1*b1     z1 = (a0+a1)*(b0+b1) - z0 - z2
  and the result is z2*B^2m + z1*B^m + z0. From O(n^2) to O(n^1.585).
  ⛔ THE THRESHOLD IS NOT A DETAIL: below it, the three calls and the additions cost more
  than the four products they avoid. The value here is MEASURED on this machine.
  ⚠️ The workspace is ONE, passed down through the recursion: allocating at every level
  would repeat the mistake this worksite has already paid for three times. }
const
  KARATSUBA_MIN = 24;
  { ⭐⭐⭐ TOOM-3. Splitting into THREE parts needs FIVE products of a third of the size
    instead of the nine of schoolbook: O(n^1.465), better than Karatsuba's 1.585. The
    count at equal n: Karatsuba does 3*(n/2)^2 = 0.75 n^2, Toom-3 does 5*(n/3)^2 = 0.56 n^2.
    ⛔ THE PRICE IS THE INTERPOLATION, and that is where it gets got wrong: five evaluation
    points (0, 1, -1, 2, infinity), EXACT divisions by 2 and by 3, and a value that can be
    NEGATIVE. The threshold is high precisely because that surround costs.

    ⛔⛔ THE THRESHOLD IS MEASURED, AND THE NUMBER IS FAR HIGHER THAN KARATSUBA'S: it is
    Toom-3's LINEAR cost (evaluation + interpolation, ~12 passes over the whole value per
    level) that decides, not the exponent. Measured 14 Aug 2026 on this machine, product of
    two n-limb numbers, best of 15 runs x 3 passes against a binary identical but for this
    constant:

      limb        320    400    512    700   1000   1400   2000
      threshold 350   -0.1%  -9.6%  -7.5%  -1.4% -10.1% -10.2%  -9.1%
      threshold 200   +2.3% -10.9%  -7.7%  -4.8%  -4.0% -11.4%  -3.4%

    ⭐ Lowering the threshold is NOT better: at 2000 limbs a threshold of 200 gives -3.4%
    where 350 gives -9.1%, because every extra Toom level pays its linear cost on
    sub-products too small to repay it. With a threshold of 130 the product at 160 limbs
    was +14.7%: a LOSS, reproduced twice.
    ⚠️ The noise floor of this measurement is ~4%: the values below 400 limbs say nothing,
    which is why the threshold sits where the gap is stable. }
  TOOM3_MIN = 350;
  { 3 * INV3 = 1 (mod 2^64): the exact division by 3 is a MULTIPLICATION. }
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

{ ⛔ The two above walk until the carry dies out, which is fine when the number is known not
  to overflow. Toom's interpolation works in TWO'S COMPLEMENT at a fixed width, where the
  carry out is DISCARDED: without a bound, on a negative value (all $FF..F) the propagation
  would walk off the end of the vector. One more bound costs a comparison; silent corruption
  costs a day. }
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

{ d = x + y and d = x - y over n limbs, returning the carry/borrow out. They exist so the
  {$IFDEF} is not repeated inside Toom's body, which is already enough to read.
  ⚠️ d MAY alias x or y: for every index it reads first and writes after. }
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

{ ---- TWO'S COMPLEMENT arithmetic at a fixed width, for the interpolation ----
  ⭐⭐⭐ THIS IS THE DECISION THAT TAKES THE SIGNS OUT OF THE WAY. Toom-3's interpolation
  passes through negative values, and the obvious route - carrying magnitude and sign at
  every step - is exactly where this is usually got wrong. Here instead every intermediate
  lives in two's complement over L limbs, with L chosen so that |value| < B^L/2: addition
  and subtraction are the unsigned ones, the carry out is discarded, and the sign exists
  only as the top bit. The EXACT divisions (by 2 and by 3) survive the modulus: the true
  quotient is congruent to the computed one and lies in range, so it IS that one.
  ⛔ The ONLY sign left is W(-1)'s, which cannot be avoided because the PRODUCT wants two
  magnitudes. It is a boolean, not an algebra. }

{ d[0..L-1] := s[0..sn-1], zero-extended (the value is non-negative). }
procedure TcSet(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var i: PtrInt;
begin
  if sn > L then sn := L;
  for i := 0 to sn - 1 do d[i] := s[i];
  for i := sn to L - 1 do d[i] := 0;
end;

{ d += s (sn limbs, zero-extended), modulo B^L. }
procedure TcAddN(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var c: QWord;
begin
  if sn > L then sn := L;
  c := RunAdd(d, d, s, sn);
  PropAddLim(@d[sn], c, L - sn);
end;

{ d -= s (sn limbs, zero-extended), modulo B^L. }
procedure TcSubN(d: PQWord; L: PtrInt; s: PQWord; sn: PtrInt);
var c: QWord;
begin
  if sn > L then sn := L;
  c := RunSub(d, d, s, sn);
  PropSubLim(@d[sn], c, L - sn);
end;

{ ⭐⭐ d -= (t shl s), in ONE pass. The obvious form - shift into a scratch vector first,
  then subtract - is TWO passes over the whole value plus one more vector. At this size the
  linear cost is not surround: it was measured eating two thirds of Toom-3's gain. }
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

{ ⭐⭐ d := (d shl 1) + s, in ONE pass: it is the Horner step of the evaluation at 2
  (A(2) = ((a2*2) + a1)*2 + a0), and for the same reason it is not done in two. }
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

{ d := d div 2, ARITHMETIC: the top bit is replicated, otherwise a negative value would
  become huge and positive. The division is exact by construction. }
procedure TcShr1(d: PQWord; L: PtrInt);
var i: PtrInt; sgn: QWord;
begin
  sgn := d[L - 1] and QWord($8000000000000000);
  for i := 0 to L - 2 do
    d[i] := (d[i] shr 1) or (d[i + 1] shl 63);
  d[L - 1] := (d[L - 1] shr 1) or sgn;
end;

{ d := d div 3, EXACT. ⭐ It is not a division: it multiplies by the inverse of 3 modulo
  2^64, limb by limb, carrying how much was "left over" - which is at most 2. It works on
  negative values too, because the modulus does not tell them apart. }
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

{ compare two magnitudes of equal length, from the top limb down }
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

  The two factors are split into THREE parts of k limbs (the last holds n2 <= k):
      a = a0 + a1*B^k + a2*B^2k          b = b0 + b1*B^k + b2*B^2k
  The product is a degree-4 polynomial in B^k:
      P(x) = c0 + c1 x + c2 x^2 + c3 x^3 + c4 x^4
  and FIVE values are enough to reconstruct its coefficients. The points are 0, 1, -1, 2 and
  "infinity" (i.e. the leading coefficient):
      W0 = a0*b0                W4 = a2*b2
      W1 = A(1)*B(1)            Wm = A(-1)*B(-1)            W2 = A(2)*B(2)

  THE INTERPOLATION, which is the part that gets got wrong, in six steps:
      tA = (W1 + Wm)/2 = c0 + c2 + c4        tB = (W1 - Wm)/2 = c1 + c3
      c2 = tA - c0 - c4
      tC = (W2 - c0 - 16 c4 - 4 c2)/2 = c1 + 4 c3
      c3 = (tC - tB)/3                       c1 = tB - c3
  ⛔ The divisions are EXACT - every numerator is divisible by construction - and must be
  done as such: a long division here would cost more than the product it saves.
  ⛔ tB, tC and their addends can be NEGATIVE: they live in two's complement over L limbs
  (see the TcXxx helpers above), and L is chosen so none of them ever reaches B^L/2.

  ⚠️ All five products are done at size k+1, including W0 and W4 which would use less: the
  recursion is written for operands of EQUAL size, and that is what keeps the index
  bookkeeping verifiable. The little that is wasted can be revisited when a measurement
  says it is worth it. }
procedure MulToom3(d, a, b, ws: PQWord; n: PtrInt);
var
  k, n2, L, i: PtrInt;
  W0, W1, WM, W2, W4, TA, TB, TC, TT, EA, EB, rest: PQWord;
  sa, sb, sm: Integer;
  c: QWord;

  { ⭐ The evaluation at -1 is the ONLY place a sign is born: a0 - a1 + a2 can be negative,
    and the product wants a magnitude. Compute |.| and report the sign; the rest of the
    interpolation knows nothing about it. }
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
      RunSub(dst, tmp, dst, k + 1);            { the other way round, and the sign }
      Result := 1;
    end;
  end;

begin
  k := (n + 2) div 3;          { ceil(n/3): the two low parts }
  n2 := n - 2 * k;             { the high part, 1 <= n2 <= k }
  L := 2 * k + 4;              { width of the intermediates, with two limbs of headroom }

  W0 := ws;            W1 := @ws[L];       WM := @ws[2 * L];
  W2 := @ws[3 * L];    W4 := @ws[4 * L];
  TA := @ws[5 * L];    TB := @ws[6 * L];   TC := @ws[7 * L];  TT := @ws[8 * L];
  EA := @ws[9 * L];    EB := @ws[9 * L + (k + 2)];
  rest := @ws[9 * L + 2 * (k + 2)];

  { --- W0 = a0*b0, and W4 = a2*b2 --- }
  TcSet(EA, k + 1, a, k);          TcSet(EB, k + 1, b, k);
  MulRec(W0, EA, EB, rest, k + 1); W0[2 * k + 2] := 0; W0[2 * k + 3] := 0;

  TcSet(EA, k + 1, @a[2 * k], n2); TcSet(EB, k + 1, @b[2 * k], n2);
  MulRec(W4, EA, EB, rest, k + 1); W4[2 * k + 2] := 0; W4[2 * k + 3] := 0;

  { --- W1 = A(1)*B(1), with A(1) = a0+a1+a2 < 3*B^k, which fits in k+1 limbs --- }
  TcSet(EA, k + 1, a, k);  TcAddN(EA, k + 1, @a[k], k);  TcAddN(EA, k + 1, @a[2 * k], n2);
  TcSet(EB, k + 1, b, k);  TcAddN(EB, k + 1, @b[k], k);  TcAddN(EB, k + 1, @b[2 * k], n2);
  MulRec(W1, EA, EB, rest, k + 1); W1[2 * k + 2] := 0; W1[2 * k + 3] := 0;

  { --- Wm = |A(-1)| * |B(-1)|, with the sign kept aside --- }
  sa := EvalMinus1(EA, TT, a);
  sb := EvalMinus1(EB, TT, b);
  sm := sa xor sb;
  MulRec(WM, EA, EB, rest, k + 1); WM[2 * k + 2] := 0; WM[2 * k + 3] := 0;

  { --- W2 = A(2)*B(2), with A(2) = a0 + 2a1 + 4a2 < 7*B^k: Horner, two doublings --- }
  TcSet(EA, k + 1, @a[2 * k], n2);
  TcShl1AddN(EA, k + 1, @a[k], k); TcShl1AddN(EA, k + 1, a, k);
  TcSet(EB, k + 1, @b[2 * k], n2);
  TcShl1AddN(EB, k + 1, @b[k], k); TcShl1AddN(EB, k + 1, b, k);
  MulRec(W2, EA, EB, rest, k + 1); W2[2 * k + 2] := 0; W2[2 * k + 3] := 0;

  { ================= interpolation ================= }
  { tA = (W1 + Wm)/2, tB = (W1 - Wm)/2 - with Wm's sign deciding the direction.
    ⭐ Written STRAIGHT into the destination: copying W1 and then adding in place were two
    passes where one is enough. }
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

  { c2 = tA - c0 - c4, and from here TA IS c2 }
  TcSubN(TA, L, W0, L);
  TcSubN(TA, L, W4, L);

  { tC = (W2 - c0 - 16 c4 - 4 c2)/2 = c1 + 4 c3 }
  RunSub(TC, W2, W0, L);
  TcSubShl(TC, W4, L, 4);
  TcSubShl(TC, TA, L, 2);
  TcShr1(TC, L);

  { c3 = (tC - tB)/3, then c1 = tB - c3 }
  TcSubN(TC, L, TB, L);
  TcDivExact3(TC, L);                 { TC is c3 }
  TcSubN(TB, L, TC, L);               { TB is c1 }

  { ================= the result is reassembled =================
    d = c0 + c1 B^k + c2 B^2k + c3 B^3k + c4 B^4k, and the lengths are NOT guessed: c1 and
    c2 are below B^(2k+1), c3 below B^(k+n2+1), c4 below B^(2*n2). ⛔ Each of those lands
    inside d's 2n limbs - the arithmetic is in the procedure's comment - and carry
    propagation is BOUNDED to what is left: a carry walking off the end of d would be
    somebody else's memory. }
  { ⭐ c0 and c4 are WRITTEN, not accumulated: they occupy exactly [0,2k) and [4k,2n),
    which are still untouched. Only the 2k limbs in the middle are left to zero. }
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

{ The workspace MulRec needs for n limbs, COMPUTED rather than estimated.
  ⛔ A generous estimate is still an estimate: below there are five pointers derived from k
  plus one level of recursion, and getting it wrong by one limb does not raise an error -
  it gives a right answer almost every time. }
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

{ d[0..2n-1] = a[0..n-1] * b[0..n-1]. ws: workspace of MulWsNeed(n) limbs. }
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
  { ⭐ Above the threshold, and with both factors large enough, it switches to Karatsuba.
    The two factors are brought to the SAME length with leading zeros: the recursion is
    written for operands of equal size, and that is what keeps the index bookkeeping simple
    - and therefore verifiable. }
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
      { Three addends on limb i+j: what is already there, the low half of the product and
        the carry. ⛔ EVERY addition can overflow, and every overflow goes into the HIGH
        half: forgetting one goes wrong only on certain values, which is the worst way. }
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
{ ⛔⛔⛔ IT BUILDS IN A SEPARATE VECTOR, and that is not fussiness. `x = x + y` brings dst
  and a here as THE SAME array: the old code did `SetLength(dst, m+1)`, which REALLOCATES,
  and the `const a` parameter went on pointing at the block just FREED. From that moment it
  reads dead memory and corrupts the heap - the symptom was a SIGSEGV inside FPC's
  SysFreeMem, much later and on an innocent operation, with every value printed up to that
  point CORRECT.
  ⚠️ It only shows when the addition MAKES dst GROW: with a number that already fits in the
  available limbs the SetLength does not reallocate and everything appears to work.
  14 Aug 2026. }
var
  t: TLimbs;
  i, m: Integer;
  s, carry, x: QWord;
begin
  m := an; if bn > m then m := bn;
  { ⭐ THE TEMPORARY ONLY WHEN IT IS NEEDED. The danger is REALLOCATION while `a` or `b`
    point at the same block as dst; if dst is already large enough nothing is reallocated
    and the write can happen in place, which is the NORMAL case inside a loop (after the
    first few turns the vector already has its size). Measured: it is the difference between
    one allocation per operation and none. }
  if (Length(dst) >= m + 1) and (LimbsRefCount(dst) <= 1) then
  begin
    {$IFDEF CPUX86_64}
    { ⭐ Equal lengths: this is exactly an ADC chain, with no branches. It is the normal case
      of accumulation, where the two operands have grown together. }
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
      dst[i] := s;      { same index read and written: the aliasing is harmless }
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
  { Same reason as BigAdd, and the same shortcut: in place when nothing is reallocated. }
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
    { estimate from two limbs over one; u[j+n] < v[n-1] is the invariant that holds DIVQ up }
    if u[j + n] >= v[n - 1] then
    begin
      qhat := QWord($FFFFFFFFFFFFFFFF);
      rhat := u[j + n - 1] + v[n - 1];
      neg := rhat < v[n - 1];      { the carry says rhat is already past one limb }
    end
    else
    begin
      qhat := DivMod128By64(u[j + n], u[j + n - 1], v[n - 1], rhat);
      neg := False;
    end;
    { correction: while qhat*v[n-2] > rhat:u[j+n-2], qhat is too high }
    while (not neg) do
    begin
      hi := MulHi64(qhat, v[n - 2]);
      lo := qhat * v[n - 2];
      if (hi > rhat) or ((hi = rhat) and (lo > u[j + n - 2])) then
      begin
        Dec(qhat);
        rhat := rhat + v[n - 1];
        if rhat < v[n - 1] then neg := True;   { rhat left the limb: correcting is enough }
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

  { the remainder is u[0..n-1] >> s }
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
