'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' regex-redux, SedaiBasic MODERN dialect - PARALLEL.
''
'' Uses SedaiBasic's REGEXCOUNT / REGEXREPLACE, which are backed by FPC's RegExpr unit. That is the
'' point of the comparison: Lua reaches for PCRE2 and Python for its "re" module, both regex engines
'' written in C, so answering with a matcher hand-written in BASIC would measure the matcher rather
'' than the language. Same weapons - each implementation with the regex engine its runtime provides.
''
'' ⭐ TEN TASKS, not nine. The nine counts are independent of each other, which is the obvious half;
'' the less obvious half is that the five-step REPLACE CHAIN is independent of all nine - it reads the
'' same stripped sequence and produces its own string. The chain cannot be split internally (each step
'' consumes the previous one's output), but it can run BESIDE the counts, so it is task number ten and
'' its length is hidden behind them rather than added to them.
''
'' ⚠️ A worker never PRINTS: it stores its answer, and the main thread emits the nine counts in order
'' and then the three lengths. The output is byte-identical to the sequential version however the ten
'' interleave.
''
'' ⛔ ProcessorCount(), not CpuCount(): the latter counts SOCKETS (1 on this machine) and using it
'' makes the program single-threaded in silence.

Dim Shared As String gSeqs
Dim Shared As String gPat(0 To 8)
Dim Shared As LongInt gCount(0 To 8)
Dim Shared As String gRp(0 To 4)
Dim Shared As String gRr(0 To 4)
Dim Shared As Integer gPostLen

'' Task 0..8: count one pattern. Task 9: run the whole replace chain.
Sub rrWorker( ByVal id As Integer )
  If id < 9 Then
    gCount(id) = RegexCount(gSeqs, gPat(id))
  Else
    Dim As String post = gSeqs
    For i As Integer = 0 To 4
      post = RegexReplace(post, gRp(i), gRr(i))
    Next i
    gPostLen = Len(post)
  End If
End Sub

Dim As String inp = ""
Dim As String chunk

'' Read in BLOCKS, the way every reference implementation does - the Python one is a single
'' stdin.buffer.read(). The line-at-a-time loop this replaces cost 532 ms of a 2005 ms program (26%)
'' and was reading the input one line at a time only because the port was written that way, not
'' because the language needs it: FreeBASIC has Input(n [, #f]) (KeyPgInputnum) and so do we.
Open Cons For Input As #1
Do While Not Eof(1)
  chunk = Input(65536, #1)
  If Len(chunk) = 0 Then Exit Do
  inp += chunk
Loop
Close #1

Dim As Integer inputLength = Len(inp)

'' Strip the sequence descriptions and the newlines. This one is on the critical path of everything
'' else, so it stays where it is.
gSeqs = RegexReplace(inp, ">.*" + Chr(10) + "|" + Chr(10), "")
Dim As Integer seqsLength = Len(gSeqs)
inp = ""

gPat(0) = "agggtaaa|tttaccct"
gPat(1) = "[cgt]gggtaaa|tttaccc[acg]"
gPat(2) = "a[act]ggtaaa|tttacc[agt]t"
gPat(3) = "ag[act]gtaaa|tttac[agt]ct"
gPat(4) = "agg[act]taaa|ttta[agt]cct"
gPat(5) = "aggg[acg]aaa|ttt[cgt]ccct"
gPat(6) = "agggt[cgt]aa|tt[acg]accct"
gPat(7) = "agggta[cgt]a|t[acg]taccct"
gPat(8) = "agggtaa[cgt]|[acg]ttaccct"

gRp(0) = "tHa[Nt]"              : gRr(0) = "<4>"
gRp(1) = "aND|caN|Ha[DS]|WaS"   : gRr(1) = "<3>"
gRp(2) = "a[NSt]|BY"            : gRr(2) = "<2>"
gRp(3) = "<[^>]*>"              : gRr(3) = "|"
gRp(4) = "\|[^|][^|]*\|"        : gRr(4) = "-"

'' Ten tasks. Even a two-core machine gains: the replace chain, which is the longest single task,
'' stops being time nobody else is using.
Dim As Any Ptr h()
ReDim h(0 To 9)
For k As Integer = 0 To 9
  h(k) = ThreadCreate( @rrWorker, k )
Next k
For k As Integer = 0 To 9
  ThreadWait( h(k) )
Next k

For i As Integer = 0 To 8
  Print gPat(i); " "; Str(gCount(i)); Chr(10);
Next i

Print Chr(10);
Print Str(inputLength); Chr(10);
Print Str(seqsLength); Chr(10);
Print Str(gPostLen); Chr(10);
