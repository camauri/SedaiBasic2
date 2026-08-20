# CLBG - SedaiBasic2 vs Python 3 vs Lua 5.4 - 20 agosto 2026

Intel Core Ultra 9 185H (16c/22t), Debian 13. Dimensione STANDARD (i valori veri della CLBG),
migliore di 3 esecuzioni, raffreddamento 20 s fra le corse.

Termometro (binario nativo fbc, dice quanto e' veloce la macchina IN QUESTO MOMENTO):
  batteria principale   44 ms in apertura, 44 ms in chiusura, deriva 0,0%
  rigiro dei banchi Lua 42 ms in apertura, 42 ms in chiusura, deriva 0,0%
Un primo rigiro Lua e' stato SCARTATO: deriva 8,5%, lo strumento stesso lo ha dichiarato non
confrontabile. I numeri qui sotto sono quelli del secondo.

Output verificato a OGNI corsa contro un riferimento: MATCH su tutti e 11 i banchi, in tutti i
profili. Un programma che muore presto sembra velocissimo.

| banco | tipo | Python | Lua | interp | AOT | JIT | AOT+JIT | Python/sb | Lua/sb |
|---|:--:|---:|---:|---:|---:|---:|---:|---:|---:|
| binary-trees | int | 8 588 ms | 23 300 ms | 27 100 ms | 26 500 ms | 109 400 ms | **26 300 ms** | 0.33× 🔴 | 0.89× 🔴 |
| fannkuch-redux | int | 50 200 ms | 202 700 ms | 264 000 ms | **27 200 ms** | 29 400 ms | **27 200 ms** | **1.85× 🟢** | **7.45× 🟢** |
| fasta | float | 12 200 ms | 10 500 ms | 21 400 ms | 7 647 ms | 8 561 ms | **7 576 ms** | **1.61× 🟢** | **1.39× 🟢** |
| k-nucleotide | string | 12 200 ms | 42 900 ms | 80 000 ms | 9 479 ms | 11 700 ms | **9 412 ms** | **1.30× 🟢** | **4.56× 🟢** |
| mandelbrot | float | 36 400 ms | 37 300 ms | 11 600 ms | 3 581 ms | **2 006 ms** | 2 148 ms | **18.15× 🟢** | **18.59× 🟢** |
| n-body | float | 125 900 ms | 60 300 ms | 35 400 ms | 3 576 ms | 3 684 ms | **3 558 ms** | **35.39× 🟢** | **16.95× 🟢** |
| pidigits | int | 603 ms | 1 347 ms | 1 304 ms | 1 333 ms | **1 300 ms** | 1 325 ms | 0.46× 🔴 | **1.04× 🟢** |
| pidigits-basic | int | 608 ms | 1 351 ms | 63 100 ms | 18 100 ms | **17 100 ms** | 18 100 ms | 0.04× 🔴 | 0.08× 🔴 |
| regex-redux | string | 709 ms | 2 031 ms | 587 ms | 591 ms | **583 ms** | 587 ms | **1.22× 🟢** | **3.48× 🟢** |
| reverse-complement | string | 614 ms | 4 649 ms | 11 800 ms | **5 812 ms** | 7 029 ms | 5 833 ms | 0.11× 🔴 | 0.80× 🔴 |
| spectral-norm | float | 39 500 ms | 33 400 ms | 8 777 ms | **801 ms** | 934 ms | 805 ms | **49.31× 🟢** | **41.70× 🟢** |

Rapporti = loro tempo / migliore di sb: sopra 1 sb e' piu' veloce.
Condizioni da riportare SEMPRE sotto la tabella: best-of-N, raffreddamento, dimensione
(STANDARD/quick), deriva del termometro, esito della verifica dell'output (MATCH/CHECK),
versioni di Python e Lua, e quali riferimenti chiamano librerie C (GMP, OpenSSL BN, PCRE2).

## Come si legge

Rapporti = tempo del riferimento / miglior tempo di sb. Sopra 1 sb e' piu' veloce.
Il migliore fra i quattro motori di sb e' in grassetto nella sua colonna.
La colonna "tipo" e' il tipo che domina il CICLO CALDO, non il conteggio delle dichiarazioni:
contare le dichiarazioni direbbe "mandelbrot: 27 int / 5 float" perche' i contatori di ciclo sono
interi, mentre il nucleo (zr*zr - zi*zi + cr) e' tutto Double. Due banchi sono misti e si e' scelto
il tipo che domina il TEMPO: fasta e' float (generatore congruenziale Double per carattere) ma
produce stringhe; k-nucleotide e' string (Mid/Asc per base) ma l'hash e' intero.

Bilancio: Python 7 vinti / 4 persi.  Lua 8 vinti / 3 persi.

## Il tipo separa i risultati meglio di qualunque altra colonna

  float   4 banchi su 4 vinti, da 1,61x a 49,31x
  string  2 vinti su 3
  int     1 vinto su 4

E i tre "int" persi non perdono per l'aritmetica: perdono per l'ALLOCAZIONE (binary-trees) e per la
PRECISIONE ARBITRARIA (i due pidigits). Lavoro che i motori di esecuzione non toccano.

## Cosa misurano davvero due righe

pidigits e regex-redux: il riferimento Python chiama GMP e PCRE2 via ctypes (e regex-redux
parallelizza su cpu_count()), quello Lua chiama OpenSSL BN e PCRE2. Su quelle righe la CLBG
confronta i BINDING, non i linguaggi. Con quel metro la nostra BigInt e' alla pari con OpenSSL
(1,04x) e il nostro motore regex batte PCRE2 con entrambi i riferimenti (1,22x e 3,48x).
pidigits-basic mette la nostra aritmetica scritta in BASIC contro quelle stesse librerie C: non e'
un confronto omologo, e il banco nasce apposta per misurare quello.

## Difetto aperto che questa batteria ha scoperto

JIT su binary-trees: 109 400 ms contro i 26 500 ms dell'AOT sullo stesso programma, e 4x peggio
dell'interprete. E' il valore piu' fuori scala della tabella. Non indagato.

## Cosa questa batteria NON misura

La correzione del cancello record/thread di oggi (aba5fb4). Il banco binary-trees esegue
binary-trees-modern-arena.bas, che non passa dal percorso dei record condivisi; la correzione e'
misurata su binary-trees-modern-1t.bas, che nella suite non c'e'.

Lua 5.4 e' l'interprete di riferimento, NON LuaJIT. I tre banchi Lua richiedono due moduli C che
Debian non impacchetta tutti: rex_pcre2 (pacchetto lua-rex-pcre2) e bn (lbn di lhf, su OpenSSL BN,
da compilare).
