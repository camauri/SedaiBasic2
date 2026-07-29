'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' fannkuch-redux, SedaiBasic MODERN dialect.
'' Translated from the Lua version (contributed by Mike Pall).
''
'' The Lua reference is the one worth translating: it is imperative and index-based, which is what
'' BASIC is. The Python version reaches the same answer through generators, islice and bytearray
'' slicing - none of which exist here, so porting it means reinventing the algorithm and then
'' debugging the reinvention. This one reads across almost line for line.
''
'' Sequential, like the Lua reference. (The Python version splits the permutation space across a
'' Pool; that shape is a separate step, to be added on top of a version known to be correct.)

Dim Shared As LongInt gSum
Dim Shared As Integer gMaxFlips

Sub fannkuch( ByVal n As Integer )
  Dim As Integer p(1 To n), q(1 To n), s(1 To n)
  Dim As Integer sign = 1, maxflips = 0
  Dim As LongInt total = 0

  For i As Integer = 1 To n
    p(i) = i : q(i) = i : s(i) = i
  Next i

  Do
    '' --- copy and flip ---
    Dim As Integer q1 = p(1)
    If q1 <> 1 Then
      For i As Integer = 2 To n
        q(i) = p(i)
      Next i
      Dim As Integer flips = 1
      Do
        Dim As Integer qq = q(q1)
        If qq = 1 Then
          total += sign * flips
          If flips > maxflips Then maxflips = flips
          Exit Do
        End If
        q(q1) = q1
        If q1 >= 4 Then
          Dim As Integer i = 2, j = q1 - 1
          Do
            Dim As Integer sw = q(i) : q(i) = q(j) : q(j) = sw
            i += 1 : j -= 1
          Loop Until i >= j
        End If
        q1 = qq
        flips += 1
      Loop
    End If

    '' --- permute ---
    If sign = 1 Then
      Dim As Integer sw = p(2) : p(2) = p(1) : p(1) = sw
      sign = -1
    Else
      Dim As Integer sw = p(2) : p(2) = p(3) : p(3) = sw
      sign = 1
      For i As Integer = 3 To n
        Dim As Integer sx = s(i)
        If sx <> 1 Then
          s(i) = sx - 1
          Exit For
        End If
        If i = n Then
          '' out of permutations
          gSum = total
          gMaxFlips = maxflips
          Exit Sub
        End If
        s(i) = i
        '' rotate 1 <- ... <- i+1
        Dim As Integer t = p(1)
        For j As Integer = 1 To i
          p(j) = p(j + 1)
        Next j
        p(i + 1) = t
      Next i
    End If
  Loop
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 7
If Len(Command(1)) > 0 Then N = CInt(Command(1))

fannkuch( N )
Print Str(gSum)
Print "Pfannkuchen("; Str(N); ") = "; Str(gMaxFlips)
