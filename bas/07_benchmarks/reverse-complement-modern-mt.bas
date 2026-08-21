'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' reverse-complement, SedaiBasic MODERN dialect - PARALLEL.
''
'' Reads FASTA on stdin, writes each sequence reversed and IUB-complemented, 60 characters to a line.
'' The sequential version (reverse-complement-modern.bas) makes one pass per sequence; the output
'' position of a character depends only on its input position, so the pass splits cleanly.
''
'' ⭐ THE CHUNK IS A MULTIPLE OF 60, and that is the whole design. A worker whose slice starts on a
'' line boundary can cut its OWN lines, so there is no second pass over the assembled sequence and no
'' serial tail - the thing that turned out to be 46% of parallel mandelbrot. The main thread only
'' prints the finished pieces in order.
''
'' ⚠️ THREE SHAPES MATTER HERE, and all three follow measurement rather than the obvious writing:
''
''  1. A worker builds its whole slice and then cuts it into lines, instead of accumulating a
''     60-character line inside the hot loop. That is what the Lua reference does (table.concat, then
''     sub()), and it is worth 26% at real N - the print is not the expensive part, being inside the
''     per-character loop is.
''
''  2. The inner statement is written exactly as in the sequential version, `buf += Mid(comp, Asc(Mid(
''     s, i, 1)) + 1, 1)` with a descending register index, because that whole expression is ONE fused
''     opcode (bcStrAppendMapped). Computing the index inline would break the match and cost far more
''     than the arithmetic saved.
''
''  3. A line is classified by Asc(ln), not by Left(ln, 1). Left() of one character ALLOCATES a
''     one-byte string and then compares strings, 339 ns per line.
''
'' ⛔ WHY THIS ONE IS SAFE TO PARALLELISE AND k-nucleotide WAS NOT (21 Aug 2026). Both read a SHARED
'' string per character. Reading a string register into a local AnsiString costs two atomic reference
'' counts per character, which several threads pay on one cache line - measured at eighteen times
'' WORSE on eight threads than on one. bcStrAppendMapped already read its strings in place; bcStrAscMid
'' did not, and was fixed. Anything added here must keep reading in place.

Dim Shared As String comp
Dim Shared As String gSeq
Dim Shared As Integer gNW
Dim Shared As Integer gCsz
Dim Shared As String gOut()

'' Build the IUB complement table once, indexed by character code. Output is always upper case,
'' whatever the case of the input - that is what the reference table does.
Sub buildComplement()
  comp = Space(256)
  For i As Integer = 0 To 255
    Mid(comp, i + 1, 1) = Chr(i)
  Next i
  Dim As String src = "ABCDGHKMNRSTVWYabcdghkmnrstvwyUu"
  Dim As String dst = "TVGHCDMKNYSABWRTVGHCDMKNYSABWRAA"
  For i As Integer = 1 To Len(src)
    Mid(comp, Asc(Mid(src, i, 1)) + 1, 1) = Mid(dst, i, 1)
  Next i
End Sub

'' One slice of the output: positions [id*gCsz+1 .. id*gCsz+gCsz] of the reversed sequence.
Sub rcWorker( ByVal id As Integer )
  Dim As Integer n = Len(gSeq)
  Dim As Integer lo = id * gCsz + 1
  If lo > n Then
    gOut(id) = ""
    Exit Sub
  End If
  Dim As Integer hi = lo + gCsz - 1
  If hi > n Then hi = n

  '' Output position j reads input position n - j + 1, so this slice walks the input DOWNWARDS from
  '' n-lo+1 to n-hi+1 - the same descending shape the sequential version uses, which is what keeps the
  '' fused opcode matching.
  Dim As String buf = ""
  Dim As Integer iFrom = n - lo + 1
  Dim As Integer iTo = n - hi + 1
  For i As Integer = iFrom To iTo Step -1
    buf += Mid(comp, Asc(Mid(gSeq, i, 1)) + 1, 1)
  Next i

  '' ...then cut THIS slice into lines. gCsz is a multiple of 60, so the slice starts on a line
  '' boundary and every line it produces is a line of the whole output. Only the final slice can end
  '' with a short one.
  Dim As Integer m = hi - lo + 1
  Dim As String outp = ""
  Dim As Integer p = 1
  Do While p <= m
    outp += Mid(buf, p, 60) + Chr(10)
    p = p + 60
  Loop
  gOut(id) = outp
End Sub

'' Emit one accumulated sequence: reversed, complemented, 60 per line.
Sub emitSequence( ByRef s As String )
  Dim As Integer n = Len(s)
  If n = 0 Then Exit Sub
  gSeq = s

  '' Ceiling division rounded UP to a multiple of 60. Never fewer than 60 characters per worker, or a
  '' short sequence would spawn threads that each cut a single line.
  Dim As Integer per = (n + gNW - 1) \ gNW
  gCsz = ((per + 59) \ 60) * 60
  If gCsz < 60 Then gCsz = 60
  Dim As Integer nw = (n + gCsz - 1) \ gCsz
  If nw < 1 Then nw = 1

  ReDim gOut(0 To nw - 1)
  Dim As Any Ptr h()
  ReDim h(0 To nw - 1)
  For k As Integer = 0 To nw - 1
    h(k) = ThreadCreate( @rcWorker, k )
  Next k
  For k As Integer = 0 To nw - 1
    ThreadWait( h(k) )
  Next k
  For k As Integer = 0 To nw - 1
    Print gOut(k);
  Next k

  gSeq = ""
  s = ""
End Sub

buildComplement()

'' One worker per LOGICAL PROCESSOR. ⛔ ProcessorCount(), not CpuCount(): the latter counts SOCKETS,
'' which is 1 on this machine, and using it makes the program single-threaded in silence.
gNW = ProcessorCount()
If gNW < 1 Then gNW = 1
If gNW > 64 Then gNW = 64

Dim As String ln, seq = ""
Open Cons For Input As #1
Do While Not Eof(1)
  Line Input #1, ln
  If Len(ln) = 0 Then Continue Do
  Dim As Integer c = Asc(ln)          '' 62 = ">", 59 = ";" - see note 3 above
  If c = 62 Then
    emitSequence(seq)
    Print ln; Chr(10);
  ElseIf c <> 59 Then
    seq += ln
  End If
Loop
Close #1
emitSequence(seq)
