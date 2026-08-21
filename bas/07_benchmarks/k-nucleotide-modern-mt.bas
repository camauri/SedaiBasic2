'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' k-nucleotide, SedaiBasic MODERN dialect - PARALLEL.
''
'' The benchmark asks seven independent questions about the same sequence: the 1-mer and 2-mer
'' frequency tables, and the count of five named fragments. Each answer needs its own full scan of the
'' sequence, and no answer depends on any other - which is why every fast entry in the game runs the
'' seven as seven tasks. The sequential version (k-nucleotide-modern.bas) runs them one after another
'' and spends seven scans of wall time.
''
'' Lua and Python both lean on a built-in hash map here (a table / a dict, both implemented in C).
'' BASIC has none, so one is built: each nucleotide packs into 2 bits, so a fragment of up to 18
'' characters is a 36-bit integer and the map can be keyed by an INTEGER instead of a string. The
'' table itself is open-addressed with linear probing. That difference is worth stating plainly: on
'' this benchmark the reference implementations are partly measuring their C hash map, and this one is
'' measuring a hash map written in BASIC.
''
'' ⛔ WHAT MADE THE PARALLEL VERSION POSSIBLE (21 Aug 2026). The hash table used to be SHARED and each
'' task cleared it before its own scan, so the seven could not overlap. Here each worker declares its
'' own table INSIDE the threaded SUB - and until this week a procedure's local array was one storage
'' for the whole program, so the seven workers would have written the same table. Local arrays now
'' have one storage per execution context.
''
'' ⚠️ A worker never PRINTS. It renders its answer into gOut(id) and the main thread prints the seven
'' in order, so the output is byte-identical to the sequential version however the workers interleave.

Const TBITS = 21               '' 2^21 slots: comfortably above the distinct 18-mers of the input

Dim Shared As String seq
Dim Shared As Integer code(0 To 255)

'' The seven tasks. gFrag(i) empty = "report the frequency table for gK(i)-mers"; otherwise it is the
'' fragment to count, and gK(i) is its length.
Dim Shared As Integer gK(0 To 6)
Dim Shared As String gFrag(0 To 6)
Dim Shared As String gOut(0 To 6)

Function keyToString( ByVal k As LongInt, ByVal nlen As Integer ) As String
  Dim As String letters = "ACGT"
  Dim As String s = ""
  For i As Integer = nlen - 1 To 0 Step -1
    s += Mid(letters, CInt((k Shr (2 * i)) And 3) + 1, 1)
  Next i
  Return s
End Function

Function stringToKey( ByVal s As String ) As LongInt
  Dim As LongInt k = 0
  For i As Integer = 1 To Len(s)
    k = (k Shl 2) Or code(Asc(Mid(s, i, 1)))
  Next i
  Return k
End Function

Sub task( ByVal id As Integer )
  Dim As Integer k = gK(id)

  '' The table is sized for THIS task, not for the largest one: 2*k+1 bits keeps the load factor at or
  '' below one half, which is what guarantees the probe loop terminates - including a lookup for a key
  '' that is absent, which a full table would spin on forever.
  Dim As Integer bits = 2 * k + 1
  If bits > TBITS Then bits = TBITS
  Dim As Integer sz = 1 Shl bits
  Dim As Integer msk = sz - 1

  '' ⛔ LOCAL, and that is the whole point of this file. One table per worker.
  Dim As LongInt hKey(0 To sz - 1)
  Dim As LongInt hCnt(0 To sz - 1)
  Dim As Integer hUsed(0 To sz - 1)

  '' --- count every k-mer of the sequence ---------------------------------------------------------
  '' The probe loop is written out here rather than called: it reads the worker's OWN table, and a SUB
  '' cannot see another procedure's locals.
  Dim As Integer n = Len(seq)
  Dim As LongInt mask = (1 Shl (2 * k)) - 1
  Dim As LongInt acc = 0
  For i As Integer = 1 To n
    acc = ((acc Shl 2) Or code(Asc(Mid(seq, i, 1)))) And mask
    If i >= k Then
      Dim As LongInt h = acc * 2654435761
      Dim As Integer p = CInt(h And msk)
      Do
        If hUsed(p) = 0 Then
          hUsed(p) = 1 : hKey(p) = acc : hCnt(p) = 1
          Exit Do
        End If
        If hKey(p) = acc Then
          hCnt(p) += 1
          Exit Do
        End If
        p = (p + 1) And msk
      Loop
    End If
  Next i

  '' --- render this task's answer -----------------------------------------------------------------
  If Len(gFrag(id)) > 0 Then
    Dim As LongInt want = stringToKey(gFrag(id))
    Dim As LongInt got = 0
    Dim As LongInt h2 = want * 2654435761
    Dim As Integer p2 = CInt(h2 And msk)
    Do
      If hUsed(p2) = 0 Then Exit Do
      If hKey(p2) = want Then
        got = hCnt(p2)
        Exit Do
      End If
      p2 = (p2 + 1) And msk
    Loop
    gOut(id) = Str(got) + Chr(9) + gFrag(id) + Chr(10)
  Else
    '' Every k-mer with its percentage, most frequent first; ties broken by name, descending.
    Dim As Integer total = 4 ^ k
    Dim As String names(0 To total - 1)
    Dim As LongInt cnts(0 To total - 1)
    For i As Integer = 0 To total - 1
      names(i) = keyToString(i, k)
      Dim As LongInt h3 = CLngInt(i) * 2654435761
      Dim As Integer p3 = CInt(h3 And msk)
      Dim As LongInt c = 0
      Do
        If hUsed(p3) = 0 Then Exit Do
        If hKey(p3) = i Then
          c = hCnt(p3)
          Exit Do
        End If
        p3 = (p3 + 1) And msk
      Loop
      cnts(i) = c
    Next i
    '' insertion sort: the sets here are 4 and 16 entries
    For i As Integer = 1 To total - 1
      Dim As String ns = names(i)
      Dim As LongInt nc = cnts(i)
      Dim As Integer j = i - 1
      Do While j >= 0
        If (cnts(j) < nc) Or ((cnts(j) = nc) And (names(j) < ns)) Then
          names(j + 1) = names(j) : cnts(j + 1) = cnts(j)
          j -= 1
        Else
          Exit Do
        End If
      Loop
      names(j + 1) = ns : cnts(j + 1) = nc
    Next i
    Dim As Double sum = Len(seq) - k + 1
    Dim As String outp = ""
    For i As Integer = 0 To total - 1
      outp += names(i) + " " + Format(cnts(i) * 100.0 / sum, "0.000") + Chr(10)
    Next i
    gOut(id) = outp + Chr(10)
  End If
End Sub

For i As Integer = 0 To 255
  code(i) = 0
Next i
code(Asc("A")) = 0 : code(Asc("a")) = 0
code(Asc("C")) = 1 : code(Asc("c")) = 1
code(Asc("G")) = 2 : code(Asc("g")) = 2
code(Asc("T")) = 3 : code(Asc("t")) = 3

'' --- read the THREE sequence from stdin ---
Dim As String line
Dim As Integer inThree = 0
Dim As String parts = ""
Open Cons For Input As #1
'' ⚠️ Classified by Asc(line), not by Left(line, 1): Left() of one character ALLOCATES a one-byte
'' string and then compares strings, 339 ns per line, and this runs once per input line. The
'' Left(line, 6) below stays as it is - it runs once per sequence header, not per line.
Do While Not Eof(1)
  Line Input #1, line
  If Len(line) = 0 Then Continue Do
  Dim As Integer c = Asc(line)        '' 62 = ">", 59 = ";"
  If c = 62 Then
    If Left(line, 6) = ">THREE" Then
      inThree = 1
    ElseIf inThree = 1 Then
      Exit Do
    End If
  ElseIf inThree = 1 Then
    If c <> 59 Then parts += UCase(line)
  End If
Loop
Close #1
seq = parts

'' The seven tasks, in the order the benchmark prints them.
gK(0) = 1  : gFrag(0) = ""
gK(1) = 2  : gFrag(1) = ""
gK(2) = 3  : gFrag(2) = "GGT"
gK(3) = 4  : gFrag(3) = "GGTA"
gK(4) = 6  : gFrag(4) = "GGTATT"
gK(5) = 12 : gFrag(5) = "GGTATTTTAATT"
gK(6) = 18 : gFrag(6) = "GGTATTTTAATTTATAGT"

'' One thread per task. There are seven, which is below any machine's live-worker ceiling, so they all
'' run at once and the wall time is the slowest single scan rather than the sum of seven.
Dim As Any Ptr h()
ReDim h(0 To 6)
For t As Integer = 0 To 6
  h(t) = ThreadCreate( @task, t )
Next t
For t As Integer = 0 To 6
  ThreadWait( h(t) )
Next t

For t As Integer = 0 To 6
  Print gOut(t);
Next t
