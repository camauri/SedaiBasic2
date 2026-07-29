'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' reverse-complement, SedaiBasic MODERN dialect.
'' Ported from the Lua version (contributed by Mike Pall, with ideas from Rici Lake).
''
'' Reads FASTA on stdin, writes each sequence reversed and IUB-complemented, 60 characters to a line.
'' The Lua reference builds its inner loop with load(); the same work is written out directly here.
''
'' Sequential: the transformation is a single pass over a stream that arrives in order.

Dim Shared As String comp

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

'' Emit one accumulated sequence: reversed, complemented, 60 per line.
Sub emitSequence( ByRef s As String )
  Dim As Integer n = Len(s)
  If n = 0 Then Exit Sub
  Dim As String outLine = ""
  Dim As Integer col = 0
  For i As Integer = n To 1 Step -1
    outLine += Mid(comp, Asc(Mid(s, i, 1)) + 1, 1)
    col += 1
    If col = 60 Then
      Print outLine; Chr(10);
      outLine = ""
      col = 0
    End If
  Next i
  If col > 0 Then Print outLine; Chr(10);
  s = ""
End Sub

buildComplement()

Dim As String line, seq = ""
Open Cons For Input As #1
Do While Not Eof(1)
  Line Input #1, line
  If Len(line) = 0 Then Continue Do
  Dim As String c = Left(line, 1)
  If c = ">" Then
    emitSequence(seq)
    Print line; Chr(10);
  ElseIf c <> ";" Then
    seq += line
  End If
Loop
Close #1
emitSequence(seq)
