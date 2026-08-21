'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' fasta, SedaiBasic MODERN dialect - PARALLEL.
''
'' The generator is a linear congruential RNG carried in one variable, x = (x*3877 + 29573) mod 139968,
'' so character k of a random sequence cannot be produced without having produced k-1. That is why the
'' sequential version (fasta-modern.bas) and the Lua reference are sequential.
''
'' ⭐ IT IS SEQUENTIAL ONLY IF YOU STEP IT ONE AT A TIME. An LCG is an affine map, and affine maps
'' COMPOSE: applying f(x)=a*x+c twice is (a*a)*x + a*c + c, which is another affine map. So the state
'' k steps ahead is one multiply and one add away from the state now, and the coefficients of that
'' k-step map are found by binary exponentiation over composition in about log2(k) steps. Each worker
'' jumps straight to the state its own slice starts at and generates from there. Nothing is shared and
'' nothing is approximated: the output is the same stream, byte for byte.
''
'' ⭐ THE SLICE IS A MULTIPLE OF 60, so a worker cuts its own lines and there is no second pass over
'' the assembled sequence - the serial tail that turned out to be 46% of parallel mandelbrot.
''
'' 📉 NOT THE BATTERY ENTRY, AND THE MEASUREMENT SAYS WHY. N=2500000, best of one:
''      interpreted  1 861 -> 1 212 ms   1.54x
''      --aot          776 -> 1 265 ms   0.61x
''      --jit          902 -> 1 331 ms   0.68x
'' Splitting the stream wins where the RNG loop is the cost and LOSES where it is not: under a
'' compiler the draws get cheap enough that building and printing 25 MB of per-worker strings becomes
'' the wall, and the sequential version - which prints each 60-character line as it makes it - never
'' builds them. The game ranks the best program per language, so benchmark.sh keeps fasta-modern.bas.
'' ⇒ Worth revisiting if the workers ever emit in ROUNDS instead of one string per slice.
''
'' ⛔ THIS PROGRAM SPAWNS THREE WAVES OF WORKERS whose SUB declares local arrays, so a per-context
'' array block is RELEASED and CLAIMED AGAIN between waves. It is the first program to do that, and it
'' found four defects in that machinery on 21 Aug 2026 - the last one being that a private descriptor
'' entry was copied from the VM-global table, which another thread can leave stale for exactly the
'' window in which this program reads it. Private entries are now built from the storage itself.
'' 🥅 job/tests/bas/bug_privarray_block_reuse_across_waves.bas is the miniature of that shape.
''
'' ⛔ ProcessorCount(), not CpuCount(): the latter counts SOCKETS (1 on this machine) and using it
'' makes the program single-threaded in silence - right answer, right output, no parallelism.

Const RNG_A = 3877
Const RNG_C = 29573
Const RNG_M = 139968

Dim Shared As LongInt lastRnd = 42
Dim Shared As Integer gNW

'' --- the task table the workers read ---------------------------------------------------------
Dim Shared As String gChars(1 To 15)     '' symbol table of the sequence being generated
Dim Shared As Double gProbs(1 To 15)     '' cumulative probabilities
Dim Shared As Integer gCnt               '' how many symbols
Dim Shared As LongInt gTotal             '' characters this call must produce
Dim Shared As LongInt gCsz               '' characters per slice (a multiple of 60)
Dim Shared As LongInt gSeed0             '' RNG state at the start of the call
Dim Shared As String gOut()

'' --- ONE line of the repeated (non-random) sequence -------------------------------------------
Dim Shared As String gRep                '' the doubled alu string
Dim Shared As Integer gRepLen            '' length of the undoubled alu

'' Coefficients of the k-step map of x -> (RNG_A*x + RNG_C) mod RNG_M, by binary exponentiation over
'' composition. Applying (a1,c1) then (a2,c2) gives (a2*a1, a2*c1 + c2), which is the whole trick.
'' Both products stay below 2^35, so a LongInt holds them exactly.
Sub jumpCoeffs( ByVal k As LongInt, ByRef ra As LongInt, ByRef rc As LongInt )
  ra = 1 : rc = 0                        '' identity map
  Dim As LongInt ba = RNG_A, bc = RNG_C  '' the one-step map
  Dim As LongInt kk = k
  Do While kk > 0
    If (kk And 1) = 1 Then
      Dim As LongInt na = (ba * ra) Mod RNG_M
      Dim As LongInt nc = (ba * rc + bc) Mod RNG_M
      ra = na : rc = nc
    End If
    Dim As LongInt sa = (ba * ba) Mod RNG_M
    Dim As LongInt sc = (ba * bc + bc) Mod RNG_M
    ba = sa : bc = sc
    kk = kk Shr 1
  Loop
End Sub

Sub randWorker( ByVal id As Integer )
  Dim As LongInt lo = CLngInt(id) * gCsz
  If lo >= gTotal Then
    gOut(id) = ""
    Exit Sub
  End If
  Dim As LongInt hi = lo + gCsz
  If hi > gTotal Then hi = gTotal

  '' Jump the RNG to the state this slice starts at: lo draws have already been made.
  Dim As LongInt ja = 0, jc = 0
  jumpCoeffs(lo, ja, jc)
  Dim As LongInt st = (ja * gSeed0 + jc) Mod RNG_M

  '' Local copies of the symbol table. Reading gChars()/gProbs() directly would be correct, but a
  '' SHARED array read is a plain load while a SHARED STRING read per character is not - see the note
  '' in k-nucleotide-modern-mt.bas. The table is 15 entries; copying it costs nothing.
  Dim As Integer cnt = gCnt
  Dim As String ch(1 To 15)
  Dim As Double pr(1 To 15)
  For j As Integer = 1 To cnt
    ch(j) = gChars(j)
    pr(j) = gProbs(j)
  Next j

  Dim As String outp = ""
  Dim As LongInt produced = lo
  Do While produced < hi
    Dim As Integer thisLine = 60
    If hi - produced < 60 Then thisLine = CInt(hi - produced)
    Dim As String line = ""
    For k As Integer = 1 To thisLine
      st = (st * RNG_A + RNG_C) Mod RNG_M
      Dim As Double r = st / 139968.0
      Dim As Integer idx = cnt
      For j As Integer = 1 To cnt
        If r < pr(j) Then
          idx = j
          Exit For
        End If
      Next j
      line += ch(idx)
    Next k
    outp += line + Chr(10)
    produced += thisLine
  Loop
  gOut(id) = outp
End Sub

Sub repWorker( ByVal id As Integer )
  Dim As LongInt lo = CLngInt(id) * gCsz
  If lo >= gTotal Then
    gOut(id) = ""
    Exit Sub
  End If
  Dim As LongInt hi = lo + gCsz
  If hi > gTotal Then hi = gTotal

  '' Position in the alu string for output character `lo` - computable directly, which is what lets
  '' this one split as cleanly as the random sequences do.
  Dim As Integer p = CInt(lo Mod gRepLen) + 1
  Dim As String outp = ""
  Dim As LongInt produced = lo
  Do While produced < hi
    Dim As Integer thisLine = 60
    If hi - produced < 60 Then thisLine = CInt(hi - produced)
    outp += Mid(gRep, p, thisLine) + Chr(10)
    p += thisLine
    If p > gRepLen Then p -= gRepLen
    produced += thisLine
  Loop
  gOut(id) = outp
End Sub

'' Split gTotal into slices that are multiples of 60 and run `which` (0 = repeat, 1 = random) on them.
Sub runSlices( ByVal which As Integer )
  Dim As LongInt per = (gTotal + gNW - 1) \ gNW
  gCsz = ((per + 59) \ 60) * 60
  If gCsz < 60 Then gCsz = 60
  Dim As Integer nw = CInt((gTotal + gCsz - 1) \ gCsz)
  If nw < 1 Then nw = 1

  ReDim gOut(0 To nw - 1)
  Dim As Any Ptr h()
  ReDim h(0 To nw - 1)
  For k As Integer = 0 To nw - 1
    If which = 0 Then
      h(k) = ThreadCreate( @repWorker, k )
    Else
      h(k) = ThreadCreate( @randWorker, k )
    End If
  Next k
  For k As Integer = 0 To nw - 1
    ThreadWait( h(k) )
  Next k
  For k As Integer = 0 To nw - 1
    Print gOut(k);
  Next k
End Sub

Sub repeatFasta( ByVal id As String, ByVal desc As String, ByVal s As String, ByVal n As LongInt )
  Print ">"; id; " "; desc; Chr(10);
  gRepLen = Len(s)
  gRep = s + s
  gTotal = n
  runSlices(0)
End Sub

Sub randomFasta( ByVal id As String, ByVal desc As String, _
                 chars() As String, probs() As Double, ByVal cnt As Integer, ByVal n As LongInt )
  Print ">"; id; " "; desc; Chr(10);
  gCnt = cnt
  For j As Integer = 1 To cnt
    gChars(j) = chars(j)
    gProbs(j) = probs(j)
  Next j
  gTotal = n
  gSeed0 = lastRnd
  runSlices(1)
  '' Leave the RNG where the sequential version would have left it: n draws on from the start.
  Dim As LongInt ja = 0, jc = 0
  jumpCoeffs(n, ja, jc)
  lastRnd = (ja * gSeed0 + jc) Mod RNG_M
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 1000
If Len(Command(1)) > 0 Then N = CInt(Command(1))

gNW = ProcessorCount()
If gNW < 1 Then gNW = 1
If gNW > 64 Then gNW = 64

Dim As String alu = _
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" + _
  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" + _
  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" + _
  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" + _
  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" + _
  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" + _
  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

Dim As String iubC(1 To 15)
Dim As Double iubP(1 To 15)
Dim As String iubChars(1 To 15) = { "a","c","g","t","B","D","H","K","M","N","R","S","V","W","Y" }
Dim As Double iubW(1 To 15) = { 0.27,0.12,0.12,0.27,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02 }
Dim As Double acc = 0
For i As Integer = 1 To 15
  iubC(i) = iubChars(i)
  acc += iubW(i)
  iubP(i) = acc
Next i

Dim As String hsC(1 To 4)
Dim As Double hsP(1 To 4)
Dim As String hsChars(1 To 4) = { "a","c","g","t" }
Dim As Double hsW(1 To 4) = { 0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008 }
acc = 0
For i As Integer = 1 To 4
  hsC(i) = hsChars(i)
  acc += hsW(i)
  hsP(i) = acc
Next i

repeatFasta( "ONE", "Homo sapiens alu", alu, N * 2 )
randomFasta( "TWO", "IUB ambiguity codes", iubC(), iubP(), 15, N * 3 )
randomFasta( "THREE", "Homo sapiens frequency", hsC(), hsP(), 4, N * 5 )
