'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' binary-trees, SedaiBasic MODERN dialect - ARENA variant.
''
'' Same algorithm and same output as binary-trees-modern.bas: every tree is really built, node by
'' node, and really walked to count it. What changes is where the nodes LIVE. Instead of one heap
'' record per node (New/Delete), the nodes of a tree are consecutive slots of an integer array and
'' a child is an INDEX rather than a pointer. Freeing a whole tree is then a single assignment -
'' the arena's top goes back to zero - instead of a full traversal calling Delete on every node.
''
'' This is what the fast C, Rust and Java entries do, so it is the fair weapon to bring: the
'' benchmark asks for the trees to be built and traversed, not for a particular allocator.
''
'' ⚠️ It measures something DIFFERENT from the pointer version, and both are worth keeping: this one
'' says where the language stands against other languages, the other one says whether our record
'' allocator is getting better. Do not let this file replace that measurement.
''
'' ⚠️ STRUCTURE: every Function/Sub comes first and ALL module-level code after, exactly as in the
'' pointer version. Splitting module code around the definitions - computing sizes at the top, then
'' declaring the routines, then starting the threads - made this program hang before printing
'' anything, with a worker body as trivial as "acc += 1". Keep the two halves separate.
''
'' Memory: the per-worker arena is sized once for the deepest tree a worker will build,
'' 2^(maxDepth+1) nodes x 2 arrays x 4 workers. At the CLBG size (N=21) that is ~134 MB.

Const NW = 4
Const NIL_IDX = -1

Dim Shared As Any Ptr mtx, cvWork, cvDone
Dim Shared As Integer gPhase, gDone, gQuit
Dim Shared As Integer gDepth              '' depth of the trees to build this phase
Dim Shared As LongInt gFrom(0 To NW - 1), gTo(0 To NW - 1)
Dim Shared As LongInt gPart(0 To NW - 1)

'' Capacity of ONE worker arena, filled in below once the depth is known.
'' ⚠️ SHARED, and it has to be: worker() reads it to find its own slice. A module-level Dim is not
'' visible inside a Sub, so an unshared wCap would be a DIFFERENT variable there, worth zero - every
'' worker would then build into slice 0, on top of each other.
Dim Shared As Integer wCap

'' Worker arenas, laid out end to end: worker k owns [k*wCap, (k+1)*wCap-1].
Dim Shared As Integer nl(), nr()
Dim Shared As Integer topIdx(0 To NW - 1)

'' The main thread's own arena, kept apart: the long-lived tree has to survive every phase, so it
'' must not share storage with anything the workers reset.
Dim Shared As Integer ml(), mr()
Dim Shared As Integer mTop

'' Build into a worker arena. Returns the index of the subtree's root.
Function makeTree( ByVal arenaBase As Integer, ByVal id As Integer, ByVal dd As Integer ) As Integer
  Dim As Integer idx = arenaBase + topIdx(id)
  topIdx(id) += 1
  If dd > 0 Then
    nl(idx) = makeTree(arenaBase, id, dd - 1)
    nr(idx) = makeTree(arenaBase, id, dd - 1)
  Else
    nl(idx) = NIL_IDX
    nr(idx) = NIL_IDX
  End If
  Return idx
End Function

Function checkTree( ByVal idx As Integer ) As LongInt
  If nl(idx) = NIL_IDX Then Return 1
  Return 1 + checkTree(nl(idx)) + checkTree(nr(idx))
End Function

'' The same two, against the main thread's arena.
Function makeTreeMain( ByVal dd As Integer ) As Integer
  Dim As Integer idx = mTop
  mTop += 1
  If dd > 0 Then
    ml(idx) = makeTreeMain(dd - 1)
    mr(idx) = makeTreeMain(dd - 1)
  Else
    ml(idx) = NIL_IDX
    mr(idx) = NIL_IDX
  End If
  Return idx
End Function

Function checkTreeMain( ByVal idx As Integer ) As LongInt
  If ml(idx) = NIL_IDX Then Return 1
  Return 1 + checkTreeMain(ml(idx)) + checkTreeMain(mr(idx))
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

    Dim As Integer arenaBase = id * wCap
    Dim As LongInt acc = 0
    For i As LongInt = gFrom(id) To gTo(id)
      topIdx(id) = 0                      '' free the previous tree: the whole point of an arena
      Dim As Integer t = makeTree(arenaBase, id, gDepth)
      acc += checkTree(t)
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

'' A complete tree of depth d holds 2^(d+1)-1 nodes.
wCap = 1
For e As Integer = 1 To maxDepth + 1
  wCap *= 2
Next e
Dim As Integer mCap = wCap * 2            '' the main thread also builds the stretch tree (maxDepth+1)

ReDim nl(0 To NW * wCap - 1)
ReDim nr(0 To NW * wCap - 1)
ReDim ml(0 To mCap - 1)
ReDim mr(0 To mCap - 1)

mTop = 0
Dim As Integer st = makeTreeMain(stretchDepth)
Print "stretch tree of depth "; Str(stretchDepth); Chr(9); " check: "; Str(checkTreeMain(st))

'' The stretch tree is dead here, so the long-lived one reuses its storage from the bottom.
mTop = 0
Dim As Integer longLived = makeTreeMain(maxDepth)

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

Print "long lived tree of depth "; Str(maxDepth); Chr(9); " check: "; Str(checkTreeMain(longLived))
