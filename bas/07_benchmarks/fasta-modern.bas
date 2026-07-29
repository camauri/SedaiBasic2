'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' fasta, SedaiBasic MODERN dialect.
'' Ported from the Lua version (contributed by Mike Pall, modified for 5.3 by Robin).
''
'' SEQUENTIAL: the whole point of the generator is a linear congruential RNG carried in a single
'' variable, so character k cannot be produced without having produced k-1. The Lua reference is
'' sequential for the same reason. (Python parallelises it only by pre-splitting the stream, which
'' is a different program.)
''
'' The Lua version builds its inner loop with load() at run time; that trick has no equivalent here,
'' so the cumulative-probability search is written out directly. Same numbers, same output.

Dim Shared As LongInt lastRnd = 42

Function nextRandom( ByVal maxv As Double ) As Double
  lastRnd = (lastRnd * 3877 + 29573) Mod 139968
  Return maxv * lastRnd / 139968.0
End Function

Sub repeatFasta( ByVal id As String, ByVal desc As String, ByVal s As String, ByVal n As LongInt )
  Print ">"; id; " "; desc; Chr(10);
  Dim As Integer sn = Len(s)
  Dim As String s2 = s + s
  Dim As Integer p = 1
  Dim As LongInt i = 60
  Do While i <= n
    Print Mid(s2, p, 60); Chr(10);
    p += 60
    If p > sn Then p -= sn
    i += 60
  Loop
  Dim As Integer tail = CInt(n Mod 60)
  If tail > 0 Then Print Mid(s2, p, tail); Chr(10);
End Sub

'' The symbol table is passed as parallel arrays: chars and CUMULATIVE probabilities.
Sub randomFasta( ByVal id As String, ByVal desc As String, _
                 chars() As String, probs() As Double, ByVal cnt As Integer, ByVal n As LongInt )
  Print ">"; id; " "; desc; Chr(10);
  Dim As LongInt produced = 0
  Do While produced < n
    Dim As Integer thisLine = 60
    If n - produced < 60 Then thisLine = CInt(n - produced)
    Dim As String line = ""
    For k As Integer = 1 To thisLine
      Dim As Double r = nextRandom(1.0)
      Dim As Integer idx = cnt
      For j As Integer = 1 To cnt
        If r < probs(j) Then
          idx = j
          Exit For
        End If
      Next j
      line += chars(idx)
    Next k
    Print line; Chr(10);
    produced += thisLine
  Loop
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 1000
If Len(Command(1)) > 0 Then N = CInt(Command(1))

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
