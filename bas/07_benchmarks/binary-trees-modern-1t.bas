'' SINGLE-THREAD variant of binary-trees-modern.bas - NW = 1, everything else identical.
'' Lua (and every other reference without native threads) runs these serially, so comparing our
'' 4-worker version against it measures the pool as much as the engine. This file is the like-for-like
'' arm: keep the two in step, and change only NW between them.
''
'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' binary-trees, SedaiBasic MODERN dialect.
'' Ported from the Python version (Antoine Pitrou / Dominique Wahli / Daniel Nanz / Joerg Baumann /
'' Jonathan Ultis), whose structure the Lua version shares.
''
'' Parallel, like the Python original: it maps the "build a tree and count it" job over a Pool, and
'' every one of those jobs is independent, so four workers split the iteration count between them.
'' Same weapons.
''
'' This benchmark is about ALLOCATION: the trees are built out of real records and thrown away.

Const NW = 1

Type Node
  left  As Node Ptr
  right As Node Ptr
End Type

Dim Shared As Any Ptr mtx, cvWork, cvDone
Dim Shared As Integer gPhase, gDone, gQuit
Dim Shared As Integer gDepth              '' depth of the trees to build this phase
Dim Shared As LongInt gFrom(0 To NW - 1), gTo(0 To NW - 1)
Dim Shared As LongInt gPart(0 To NW - 1)

Function makeTree( ByVal dd As Integer ) As Node Ptr
  Dim As Node Ptr t = New Node
  If dd > 0 Then
    t->left  = makeTree(dd - 1)
    t->right = makeTree(dd - 1)
  Else
    t->left  = 0
    t->right = 0
  End If
  Return t
End Function

Function checkTree( ByVal t As Node Ptr ) As LongInt
  If t->left = 0 Then Return 1
  Return 1 + checkTree(t->left) + checkTree(t->right)
End Function

Sub freeTree( ByVal t As Node Ptr )
  If t->left <> 0 Then
    freeTree(t->left)
    freeTree(t->right)
  End If
  Delete t
End Sub

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

    Dim As LongInt acc = 0
    For i As LongInt = gFrom(id) To gTo(id)
      Dim As Node Ptr t = makeTree(gDepth)
      acc += checkTree(t)
      freeTree(t)
    Next i
    gPart(id) = acc

    MutexLock mtx
    gDone += 1
    CondBroadcast cvDone
    MutexUnlock mtx
  Loop
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 10
If Len(Command(1)) > 0 Then N = CInt(Command(1))

Dim As Integer minDepth = 4
Dim As Integer maxDepth = N
If maxDepth < minDepth + 2 Then maxDepth = minDepth + 2
Dim As Integer stretchDepth = maxDepth + 1

Dim As Node Ptr st = makeTree(stretchDepth)
Print "stretch tree of depth "; Str(stretchDepth); Chr(9); " check: "; Str(checkTree(st))
freeTree(st)

Dim As Node Ptr longLived = makeTree(maxDepth)

mtx = MutexCreate() : cvWork = CondCreate() : cvDone = CondCreate()
gPhase = 0 : gDone = 0 : gQuit = 0
Dim As Any Ptr h(0 To NW - 1)
For k As Integer = 0 To NW - 1
  h(k) = ThreadCreate( @worker, k )
Next k

Dim As Integer mmd = maxDepth + minDepth
For dd As Integer = minDepth To stretchDepth - 1 Step 2
  Dim As LongInt iterations = 1
  For e As Integer = 1 To mmd - dd
    iterations *= 2
  Next e

  gDepth = dd
  Dim As LongInt span = iterations \ NW
  For k As Integer = 0 To NW - 1
    gFrom(k) = k * span + 1
    If k = NW - 1 Then gTo(k) = iterations Else gTo(k) = (k + 1) * span
    gPart(k) = 0
  Next k

  MutexLock mtx
  gDone = 0 : gPhase += 1
  CondBroadcast cvWork
  Do While gDone < NW
    CondWait cvDone, mtx
  Loop
  MutexUnlock mtx

  Dim As LongInt check = 0
  For k As Integer = 0 To NW - 1
    check += gPart(k)
  Next k
  Print Str(iterations); Chr(9); " trees of depth "; Str(dd); Chr(9); " check: "; Str(check)
Next dd

MutexLock mtx
gQuit = 1
CondBroadcast cvWork
MutexUnlock mtx
For k As Integer = 0 To NW - 1
  ThreadWait( h(k) )
Next k
MutexDestroy mtx : CondDestroy cvWork : CondDestroy cvDone

Print "long lived tree of depth "; Str(maxDepth); Chr(9); " check: "; Str(checkTree(longLived))
freeTree(longLived)
