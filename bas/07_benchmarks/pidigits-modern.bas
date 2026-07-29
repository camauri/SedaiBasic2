'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' pidigits, SedaiBasic MODERN dialect.
'' Ported from the Lua version (translated from Mr Ledrug's C program by Jeremy Zerfas,
'' transliterated from GMP to bn by Isaac Gouy).
''
'' Sequential, like every reference implementation: the spigot carries state from one digit to the next.
''
'' Arbitrary-precision arithmetic is written out here, as it is for Lua (whose reference uses the "bn"
'' library rather than a native type). Only a handful of operations are needed and most take a SMALL
'' second operand, so the whole thing is a few dozen lines over a base-10^9 limb array:
''   mulSmall / addBig / subBig / cmpBig / divDigit
'' The digit quotient is searched rather than computed: extractDigit always yields 0..9, so trying the
'' ten candidates is both simpler and faster than a general long division.
''
'' ACC CARRIES A SIGN. After a digit is eliminated the spigot leaves acc negative (the very first step
'' does: 6 - 3*3 = -3) and only climbs back above zero a few terms later. The reference implementations
'' get this for free from GMP/bn; here acc is a magnitude plus an explicit accSgn, and the two places
'' that change it - "acc += num*2" and "acc -= den*d" - go through accAddPos/accSubPos. den and num
'' are always positive, so nothing else needs a sign.

Const BASE = 1000000000        '' 10^9 - a limb product stays inside 64 bits for the small multipliers here
Const LIMBS = 4096             '' ~36k decimal digits of headroom

'' A bignum is (array, used-length). They live as shared arrays because BASIC has no by-value struct
'' that would make the arithmetic readable.
Dim Shared As LongInt acc(0 To LIMBS-1), den(0 To LIMBS-1), num(0 To LIMBS-1)
Dim Shared As LongInt tmp(0 To LIMBS-1), tmp2(0 To LIMBS-1), tmp3(0 To LIMBS-1)
Dim Shared As Integer accN, denN, numN, tmpN, tmp2N, tmp3N
Dim Shared As Integer accSgn = 1     '' sign of acc; den and num are always positive

Sub setSmall( a() As LongInt, ByRef n As Integer, ByVal v As LongInt )
  n = 0
  Do
    a(n) = v Mod BASE
    v = v \ BASE
    n += 1
  Loop While v > 0
End Sub

'' a *= k  (k small)
Sub mulSmall( a() As LongInt, ByRef n As Integer, ByVal k As LongInt )
  Dim As LongInt carry = 0
  For i As Integer = 0 To n - 1
    Dim As LongInt p = a(i) * k + carry
    a(i) = p Mod BASE
    carry = p \ BASE
  Next i
  Do While carry > 0
    a(n) = carry Mod BASE
    carry = carry \ BASE
    n += 1
  Loop
  '' k = 0 wipes every limb; trim so a length always describes a normalised magnitude (cmpBig
  '' compares lengths first, so a leading zero limb would make a smaller number compare greater).
  Do While (n > 1) And (a(n - 1) = 0)
    n -= 1
  Loop
End Sub

'' dst = a + b
Sub addBig( dst() As LongInt, ByRef dn As Integer, a() As LongInt, ByVal an As Integer, _
            b() As LongInt, ByVal bn As Integer )
  Dim As Integer m = an
  If bn > m Then m = bn
  Dim As LongInt carry = 0
  For i As Integer = 0 To m - 1
    Dim As LongInt s = carry
    If i < an Then s += a(i)
    If i < bn Then s += b(i)
    dst(i) = s Mod BASE
    carry = s \ BASE
  Next i
  dn = m
  Do While carry > 0
    dst(dn) = carry Mod BASE
    carry = carry \ BASE
    dn += 1
  Loop
End Sub

'' dst = a - b, magnitudes only; the caller guarantees a >= b.
Sub subBig( dst() As LongInt, ByRef dn As Integer, a() As LongInt, ByVal an As Integer, _
            b() As LongInt, ByVal bn As Integer )
  Dim As LongInt borrow = 0
  For i As Integer = 0 To an - 1
    Dim As LongInt v = a(i) - borrow
    If i < bn Then v -= b(i)
    If v < 0 Then
      v += BASE
      borrow = 1
    Else
      borrow = 0
    End If
    dst(i) = v
  Next i
  dn = an
  Do While (dn > 1) And (dst(dn - 1) = 0)
    dn -= 1
  Loop
End Sub

'' -1 / 0 / 1
Function cmpBig( a() As LongInt, ByVal an As Integer, b() As LongInt, ByVal bn As Integer ) As Integer
  If an <> bn Then
    If an < bn Then Return -1 Else Return 1
  End If
  For i As Integer = an - 1 To 0 Step -1
    If a(i) <> b(i) Then
      If a(i) < b(i) Then Return -1 Else Return 1
    End If
  Next i
  Return 0
End Function

Sub copyBig( dst() As LongInt, ByRef dn As Integer, a() As LongInt, ByVal an As Integer )
  For i As Integer = 0 To an - 1
    dst(i) = a(i)
  Next i
  dn = an
End Sub

'' acc += t, with t positive. Scratch: tmp3.
Sub accAddPos( t() As LongInt, ByVal tn As Integer )
  If accSgn > 0 Then
    addBig( tmp3(), tmp3N, acc(), accN, t(), tn )
  ElseIf cmpBig( acc(), accN, t(), tn ) >= 0 Then
    subBig( tmp3(), tmp3N, acc(), accN, t(), tn )       '' still negative
  Else
    subBig( tmp3(), tmp3N, t(), tn, acc(), accN )
    accSgn = 1
  End If
  copyBig( acc(), accN, tmp3(), tmp3N )
  If (accN = 1) And (acc(0) = 0) Then accSgn = 1
End Sub

'' acc -= t, with t positive. Scratch: tmp3.
Sub accSubPos( t() As LongInt, ByVal tn As Integer )
  If accSgn < 0 Then
    addBig( tmp3(), tmp3N, acc(), accN, t(), tn )       '' still negative
  ElseIf cmpBig( acc(), accN, t(), tn ) >= 0 Then
    subBig( tmp3(), tmp3N, acc(), accN, t(), tn )
  Else
    subBig( tmp3(), tmp3N, t(), tn, acc(), accN )
    accSgn = -1
  End If
  copyBig( acc(), accN, tmp3(), tmp3N )
  If (accN = 1) And (acc(0) = 0) Then accSgn = 1
End Sub

'' The digit q with den*q <= a < den*(q+1), for q in 0..9.
Function divDigit( a() As LongInt, ByVal an As Integer, b() As LongInt, ByVal bn As Integer ) As Integer
  Dim As LongInt probe(0 To LIMBS-1)
  Dim As Integer pn
  For q As Integer = 9 To 0 Step -1
    copyBig( probe(), pn, b(), bn )
    mulSmall( probe(), pn, q )
    If cmpBig( probe(), pn, a(), an ) <= 0 Then Return q
  Next q
  Return 0
End Function

'' (num*nth + acc) \ den
Function extractDigit( ByVal nth As LongInt ) As Integer
  copyBig( tmp(), tmpN, num(), numN )
  mulSmall( tmp(), tmpN, nth )
  addBig( tmp2(), tmp2N, tmp(), tmpN, acc(), accN )
  Return divDigit( tmp2(), tmp2N, den(), denN )
End Function

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 27
If Len(Command(1)) > 0 Then N = CInt(Command(1))

setSmall( acc(), accN, 0 )
setSmall( den(), denN, 1 )
setSmall( num(), numN, 1 )

Dim As Integer i = 0, k = 0
Dim As String outBuf = ""
Do While i < N
  k += 1
  '' nextTerm(k): acc = (acc + num*2) * (2k+1) ; den *= (2k+1) ; num *= k
  Dim As LongInt k2 = 2 * k + 1
  copyBig( tmp(), tmpN, num(), numN )
  mulSmall( tmp(), tmpN, 2 )
  accAddPos( tmp(), tmpN )
  mulSmall( acc(), accN, k2 )        '' k2 > 0, so the sign is unchanged
  mulSmall( den(), denN, k2 )
  mulSmall( num(), numN, k )

  '' num is positive, so a negative acc is always the smaller one.
  If accSgn < 0 Then Continue Do
  If cmpBig( num(), numN, acc(), accN ) > 0 Then Continue Do

  Dim As Integer d = extractDigit(3)
  If d <> extractDigit(4) Then Continue Do

  outBuf += Str(d)
  i += 1
  If (i Mod 10) = 0 Then
    Print outBuf; Chr(9); ":"; Str(i); Chr(10);
    outBuf = ""
  End If

  '' eliminateDigit(d): acc = (acc - den*d) * 10 ; num *= 10
  copyBig( tmp(), tmpN, den(), denN )
  mulSmall( tmp(), tmpN, d )
  accSubPos( tmp(), tmpN )
  mulSmall( acc(), accN, 10 )        '' 10 > 0, so the sign is unchanged
  mulSmall( num(), numN, 10 )
Loop

If (i Mod 10) <> 0 Then
  Print outBuf; Space(10 - (N Mod 10)); Chr(9); ":"; Str(N); Chr(10);
End If
