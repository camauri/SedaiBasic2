'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' k-nucleotide, SedaiBasic MODERN dialect.
'' Ported from the Lua version (contributed by Mike Pall).
''
'' Lua and Python both lean on a built-in hash map here (a table / a dict, both implemented in C).
'' BASIC has none, so one is built: each nucleotide packs into 2 bits, so a fragment of up to 18
'' characters is a 36-bit integer and the map can be keyed by an INTEGER instead of a string. The
'' table itself is open-addressed with linear probing.
''
'' That difference is worth stating plainly: on this benchmark the reference implementations are
'' partly measuring their C hash map, and this one is measuring a hash map written in BASIC.

Const TSIZE = 2097152          '' power of two, comfortably above the distinct 18-mers of the input
Const TMASK = TSIZE - 1

Dim Shared As LongInt hKey(0 To TSIZE - 1)
Dim Shared As LongInt hCnt(0 To TSIZE - 1)
Dim Shared As Integer hUsed(0 To TSIZE - 1)

Sub tableClear()
  For i As Integer = 0 To TSIZE - 1
    hUsed(i) = 0
  Next i
End Sub

'' Add one to the count of key k. Open addressing, linear probing.
Sub tableBump( ByVal k As LongInt )
  Dim As LongInt h = k * 2654435761
  Dim As Integer p = CInt(h And TMASK)
  Do
    If hUsed(p) = 0 Then
      hUsed(p) = 1 : hKey(p) = k : hCnt(p) = 1
      Exit Sub
    End If
    If hKey(p) = k Then
      hCnt(p) += 1
      Exit Sub
    End If
    p = (p + 1) And TMASK
  Loop
End Sub

Function tableGet( ByVal k As LongInt ) As LongInt
  Dim As LongInt h = k * 2654435761
  Dim As Integer p = CInt(h And TMASK)
  Do
    If hUsed(p) = 0 Then Return 0
    If hKey(p) = k Then Return hCnt(p)
    p = (p + 1) And TMASK
  Loop
End Function

Dim Shared As String seq
Dim Shared As Integer code(0 To 255)

'' Count every k-mer of the sequence into the table.
Sub countFrames( ByVal k As Integer )
  tableClear()
  Dim As Integer n = Len(seq)
  Dim As LongInt mask = (1 Shl (2 * k)) - 1
  Dim As LongInt acc = 0
  For i As Integer = 1 To n
    acc = ((acc Shl 2) Or code(Asc(Mid(seq, i, 1)))) And mask
    If i >= k Then tableBump(acc)
  Next i
End Sub

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

'' Print every k-mer with its percentage, most frequent first; ties broken by name, descending.
Sub frequency( ByVal k As Integer )
  countFrames(k)
  Dim As Integer total = 4 ^ k
  Dim As String names(0 To total - 1)
  Dim As LongInt cnts(0 To total - 1)
  For i As Integer = 0 To total - 1
    names(i) = keyToString(i, k)
    cnts(i) = tableGet(i)
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
  For i As Integer = 0 To total - 1
    Print names(i); " "; Format(cnts(i) * 100.0 / sum, "0.000"); Chr(10);
  Next i
  Print Chr(10);
End Sub

Sub countFragment( ByVal frag As String )
  countFrames(Len(frag))
  Print Str(tableGet(stringToKey(frag))); Chr(9); frag; Chr(10);
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
Do While Not Eof(1)
  Line Input #1, line
  If Len(line) = 0 Then Continue Do
  If Left(line, 1) = ">" Then
    If Left(line, 6) = ">THREE" Then
      inThree = 1
    ElseIf inThree = 1 Then
      Exit Do
    End If
  ElseIf inThree = 1 Then
    If Left(line, 1) <> ";" Then parts += UCase(line)
  End If
Loop
Close #1
seq = parts

frequency(1)
frequency(2)
countFragment("GGT")
countFragment("GGTA")
countFragment("GGTATT")
countFragment("GGTATTTTAATT")
countFragment("GGTATTTTAATTTATAGT")
