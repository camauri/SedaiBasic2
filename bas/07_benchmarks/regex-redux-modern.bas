'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' regex-redux, SedaiBasic MODERN dialect.
'' Ported from the Lua version (contributed by Jeremy Zerfas).
''
'' Uses SedaiBasic's REGEXCOUNT / REGEXREPLACE, which are backed by FPC's RegExpr unit. That is the
'' point of the comparison: Lua reaches for PCRE2 and Python for its "re" module, both regex engines
'' written in C, so answering with a matcher hand-written in BASIC would measure the matcher rather
'' than the language. Same weapons - each implementation with the regex engine its runtime provides.
''
'' Sequential: the replacements are applied one after another, each to the output of the last.

Dim As String inp = ""
Dim As String line

Open Cons For Input As #1
Do While Not Eof(1)
  Line Input #1, line
  inp += line + Chr(10)
Loop
Close #1

Dim As Integer inputLength = Len(inp)

'' Strip the sequence descriptions and the newlines.
Dim As String seqs = RegexReplace(inp, ">.*" + Chr(10) + "|" + Chr(10), "")
Dim As Integer seqsLength = Len(seqs)

Dim As String pat(0 To 8) = { _
  "agggtaaa|tttaccct", _
  "[cgt]gggtaaa|tttaccc[acg]", _
  "a[act]ggtaaa|tttacc[agt]t", _
  "ag[act]gtaaa|tttac[agt]ct", _
  "agg[act]taaa|ttta[agt]cct", _
  "aggg[acg]aaa|ttt[cgt]ccct", _
  "agggt[cgt]aa|tt[acg]accct", _
  "agggta[cgt]a|t[acg]taccct", _
  "agggtaa[cgt]|[acg]ttaccct" }

For i As Integer = 0 To 8
  Print pat(i); " "; Str(RegexCount(seqs, pat(i))); Chr(10);
Next i

Dim As String rp(0 To 4) = { "tHa[Nt]", "aND|caN|Ha[DS]|WaS", "a[NSt]|BY", "<[^>]*>", "\|[^|][^|]*\|" }
Dim As String rr(0 To 4) = { "<4>", "<3>", "<2>", "|", "-" }

Dim As String post = seqs
For i As Integer = 0 To 4
  post = RegexReplace(post, rp(i), rr(i))
Next i

Print Chr(10);
Print Str(inputLength); Chr(10);
Print Str(seqsLength); Chr(10);
Print Str(Len(post)); Chr(10);
