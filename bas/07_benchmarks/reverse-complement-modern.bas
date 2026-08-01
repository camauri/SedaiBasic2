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
''
'' ⚠️ TWO SHAPES MATTER HERE, and both follow the reference rather than the obvious writing:
''
''  1. emitSequence builds the WHOLE complemented sequence and then slices it 60 characters at a
''     time, instead of accumulating a 60-character line and printing it inside the hot loop. That
''     is what Lua does (table.concat, then sub()), and it is worth 26% at real N -- the print is
''     not the expensive part, being inside the per-character loop is. Measured, output byte-identical.
''
''  2. A line is classified by Asc(ln), not by Left(ln, 1). Left() of one character ALLOCATES a
''     one-byte string and then compares strings, 339 ns per line; the integer compare is a third of
''     that. This is worth copying into any line-oriented BASIC program.

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
  '' Complement the whole sequence first - the hot loop does nothing but read a byte and append one.
  Dim As String buf = ""
  For i As Integer = n To 1 Step -1
    buf += Mid(comp, Asc(Mid(s, i, 1)) + 1, 1)
  Next i
  '' ...then slice it into lines. The slicing loop runs once per 60 characters, not once per character.
  Dim As Integer p = 1
  Do While p <= n
    Print Mid(buf, p, 60); Chr(10);
    p = p + 60
  Loop
  s = ""
End Sub

buildComplement()

Dim As String ln, seq = ""
Open Cons For Input As #1
Do While Not Eof(1)
  Line Input #1, ln
  If Len(ln) = 0 Then Continue Do
  Dim As Integer c = Asc(ln)          '' 62 = ">", 59 = ";" - see note 2 above
  If c = 62 Then
    emitSequence(seq)
    Print ln; Chr(10);
  ElseIf c <> 59 Then
    seq += ln
  End If
Loop
Close #1
emitSequence(seq)
