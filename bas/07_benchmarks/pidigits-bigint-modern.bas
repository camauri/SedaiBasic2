'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' pidigits, SedaiBasic MODERN dialect, on the native BigInt type.
'' Ported from the Lua version (translated from Mr Ledrug's C program by Jeremy Zerfas,
'' transliterated from GMP to bn by Isaac Gouy).
''
'' Sequential, like every reference implementation: the spigot carries state from one digit to the next.
''
'' WHY THIS FILE EXISTS ALONGSIDE pidigits-modern.bas
''
'' The two are the SAME algorithm and answer two DIFFERENT questions, so the suite times both.
''   pidigits-basic  (pidigits-modern.bas)  writes the arbitrary-precision arithmetic out by hand over
''                   a base-10^9 limb array - about 245 lines of BASIC. What it measures is the ENGINE:
''                   how fast this language runs a long, loop-heavy, array-heavy program.
''   pidigits        (this file)            asks the language for the arithmetic. What it measures is
''                   the LIBRARY, and it is the comparison the reference invites: the Python entry does
''                   not use CPython's own integers either, it calls GMP through ctypes.
''
'' Comparing our hand-written arithmetic against someone else's GMP measures neither thing cleanly. It
'' is worth having both numbers, and worth not mistaking one for the other.
''
'' WHAT THE TYPE TAKES CARE OF
''
'' ACC CARRIES A SIGN, and that is the visible difference between the two files. After a digit is
'' eliminated the spigot leaves acc negative (the very first step does: 6 - 3*3 = -3), and it climbs
'' back above zero only a few terms later. The hand-written version keeps a magnitude plus an explicit
'' accSgn and routes the two places that change it through accAddPos/accSubPos. Here `acc < 0` is just
'' a comparison, because a BigInt is a signed value rather than a magnitude.
''
'' There is no capacity constant either. The hand-written version has to DECLARE how many limbs the
'' denominator will ever need - and getting that wrong is silent, because a MODERN out-of-range array
'' store is dropped to keep memory safe, so the top limbs vanish and the digits quietly go wrong. A
'' BigInt grows on its own, so the question does not arise.
''
'' EVERY MULTIPLICATION HERE HAS A SMALL SECOND OPERAND - k2, k, q, 3, 4, 10, d - and that is not an
'' accident of the port: it is the shape of the spigot. It matters when reading a profile, because it
'' means this program never performs a big-by-big product, and therefore says nothing about the
'' general multiplication algorithm. (Karatsuba and Toom-3 are measured elsewhere, on a program that
'' actually executes them.)
''
'' The digit quotient is SEARCHED rather than computed: extractDigit always yields 0..9, so trying the
'' ten candidates costs less than a general long division - the same choice the reference makes.

Dim As Integer N = 27
If Len(Command(1)) > 0 Then N = CInt(Command(1))

Dim acc As BigInt = 0
Dim den As BigInt = 1
Dim num As BigInt = 1
Dim tmp As BigInt = 0
Dim probe As BigInt = 0
Dim As Integer i = 0, k = 0, d, d4, q, k2
Dim As String outBuf = ""

Do While i < N
  '' One term of the series. den and num grow without bound; acc is the one that dips negative.
  k += 1
  k2 = 2 * k + 1
  acc = (acc + num * 2) * k2
  den = den * k2
  num = num * k

  '' Nothing can be extracted yet while acc is still below zero, or while num has not been overtaken.
  If acc < 0 Then Continue Do
  If num > acc Then Continue Do

  '' extractDigit(3): the candidate digit for this term.
  tmp = num * 3 + acc
  d = 0
  For q = 9 To 0 Step -1
    probe = den * q
    If probe <= tmp Then
      d = q
      Exit For
    End If
  Next q

  '' extractDigit(4) must agree, otherwise the digit is not yet settled and another term is needed.
  tmp = num * 4 + acc
  d4 = 0
  For q = 9 To 0 Step -1
    probe = den * q
    If probe <= tmp Then
      d4 = q
      Exit For
    End If
  Next q

  If d <> d4 Then Continue Do

  '' The reference prints ten digits per line, each followed by the running count.
  outBuf += Str(d)
  i += 1
  If (i Mod 10) = 0 Then
    Print outBuf; Chr(9); ":"; Str(i); Chr(10);
    outBuf = ""
  End If

  '' eliminateDigit: fold the digit out and shift the spigot one decimal place along.
  acc = (acc - den * d) * 10
  num = num * 10
Loop

'' A final short line is padded so the count still lands in the same column.
If (i Mod 10) <> 0 Then
  Print outBuf; Space(10 - (N Mod 10)); Chr(9); ":"; Str(N); Chr(10);
End If
