'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' spectral-norm, SedaiBasic MODERN dialect.
'' Ported from the Python version (Sebastien Loisel / Isaac Gouy / Josh Goldfoot /
'' Simon Descarpentries / Vadim Zelenin / Jason Stitt / Matt Vollrath / Adam Beckmeyer).
''
'' Parallel, like the Python original: that one spreads both halves of multiply_AtAv over a
'' Pool of 4 processes, so this one spreads them over 4 threads. Same weapons.
''
'' The pool is PERSISTENT. Each power iteration runs two phases and there are ten of them, so
'' forty phases in all: creating four threads per phase would cost more than the arithmetic.
'' The workers are started once and released through a barrier, which is what Pool does too.

Const NW = 4          '' workers, matching Pool(processes=4) in the Python original

Dim Shared As Integer nSize
Dim Shared As Any Ptr mtx, cvWork, cvDone
Dim Shared As Integer gPhase, gDone, gQuit
Dim Shared As Integer gOp             '' 0 = A_sum (row-major), 1 = At_sum (column-major)
Dim Shared As Double  a(), b()        '' the phase reads a(), writes b()

'' A(i,j) = (i+j)(i+j+1)/2 + i + 1 -- the infinite matrix, evaluated rather than stored.
Function evalA( ByVal i As Integer, ByVal j As Integer ) As Double
  Dim As Integer ij = i + j
  Return ij * (ij + 1) \ 2 + i + 1
End Function

Sub worker( ByVal id As Integer )
  Dim As Integer seen = 0
  Do
    MutexLock mtx
    Do While (gPhase = seen) And (gQuit = 0)
      CondWait cvWork, mtx
    Loop
    If gQuit <> 0 Then
      MutexUnlock mtx
      Exit Do
    End If
    seen = gPhase
    MutexUnlock mtx

    '' Contiguous slice of the output rows; the last worker takes the remainder.
    Dim As Integer span = nSize \ NW
    Dim As Integer lo = id * span
    Dim As Integer hi = lo + span - 1
    If id = NW - 1 Then hi = nSize - 1

    If gOp = 0 Then
      For i As Integer = lo To hi
        Dim As Double s = 0
        For j As Integer = 0 To nSize - 1
          s += a(j) / evalA(i, j)
        Next j
        b(i) = s
      Next i
    Else
      For i As Integer = lo To hi
        Dim As Double s = 0
        For j As Integer = 0 To nSize - 1
          s += a(j) / evalA(j, i)
        Next j
        b(i) = s
      Next i
    End If

    MutexLock mtx
    gDone += 1
    CondBroadcast cvDone
    MutexUnlock mtx
  Loop
End Sub

'' Release the workers into one phase and wait for every one of them.
Sub runPhase( ByVal op As Integer )
  gOp = op
  MutexLock mtx
  gDone = 0
  gPhase += 1
  CondBroadcast cvWork
  Do While gDone < NW
    CondWait cvDone, mtx
  Loop
  MutexUnlock mtx
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 5500
If Len(Command(1)) > 0 Then N = CInt(Command(1))

nSize = N
ReDim a(0 To N - 1)
ReDim b(0 To N - 1)

Dim As Double u(0 To N - 1), v(0 To N - 1), tmp(0 To N - 1)
For i As Integer = 0 To N - 1
  u(i) = 1
Next i

mtx = MutexCreate() : cvWork = CondCreate() : cvDone = CondCreate()
gPhase = 0 : gDone = 0 : gQuit = 0
Dim As Any Ptr h(0 To NW - 1)
For k As Integer = 0 To NW - 1
  h(k) = ThreadCreate( @worker, k )
Next k

For iter As Integer = 1 To 10
  '' v = A'Av
  For i As Integer = 0 To N - 1 : a(i) = u(i) : Next i
  runPhase(0)
  For i As Integer = 0 To N - 1 : a(i) = b(i) : Next i
  runPhase(1)
  For i As Integer = 0 To N - 1 : v(i) = b(i) : Next i
  '' u = A'Av
  For i As Integer = 0 To N - 1 : a(i) = v(i) : Next i
  runPhase(0)
  For i As Integer = 0 To N - 1 : a(i) = b(i) : Next i
  runPhase(1)
  For i As Integer = 0 To N - 1 : u(i) = b(i) : Next i
Next iter

MutexLock mtx
gQuit = 1
CondBroadcast cvWork
MutexUnlock mtx
For k As Integer = 0 To NW - 1
  ThreadWait( h(k) )
Next k
MutexDestroy mtx : CondDestroy cvWork : CondDestroy cvDone

Dim As Double vBv = 0, vv = 0
For i As Integer = 0 To N - 1
  vBv += u(i) * v(i)
  vv  += v(i) * v(i)
Next i
Print Using "#.#########"; Sqr(vBv / vv)
