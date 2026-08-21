'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' fannkuch-redux, SedaiBasic MODERN dialect - PARALLEL.
''
'' The sequential version (fannkuch-redux-modern.bas) walks the permutations one after another, which
'' is the Lua reference's shape. Every fast entry in the game splits the permutation space instead:
'' each worker is given a contiguous range of permutation INDICES, decodes the permutation that sits
'' at the start of its range straight from the index (factorial number system), and walks its own
'' range from there. No worker needs to know what any other is doing, and the checksum adds up
'' because the sign of a permutation's contribution is decided by the parity of its index.
''
'' ⛔ THIS PROGRAM COULD NOT BE WRITTEN BEFORE 21 AUG 2026. Each worker declares its own p(), pp() and
'' cnt() inside the threaded SUB, and a procedure's local array was a static index into the VM's one
'' array table - so two workers shared the storage and overwrote each other. It gave the right answer
'' with one worker and ran forever with two. Local arrays now have one storage per execution context,
'' and this is the first program in the corpus that depends on it.
''
'' ⛔ NO RECURSION anywhere here, deliberately: an array local to a RECURSIVE procedure is still one
'' storage for every level (job/tests/bas/bug_local_array_shared_across_recursion.bas). The algorithm
'' is iterative anyway, so this costs nothing - but it is a constraint, not a coincidence.

Dim Shared As Integer gN
Dim Shared As Integer gNW
Dim Shared As LongInt gFact(0 To 20)     '' gFact(i) = i!  (20! is the last that fits a LongInt)
Dim Shared As LongInt gChunk
Dim Shared As LongInt gTotal
Dim Shared As LongInt gChk()             '' per-worker checksum
Dim Shared As Integer gMax()             '' per-worker maximum flip count

Sub worker( ByVal id As Integer )
  Dim As Integer n = gN
  Dim As LongInt idxMin = CLngInt(id) * gChunk
  Dim As LongInt idxMax = idxMin + gChunk
  If idxMax > gTotal Then idxMax = gTotal
  If idxMin >= idxMax Then
    gChk(id) = 0
    gMax(id) = 0
    Exit Sub
  End If

  '' Each worker's own working arrays. ⛔ The whole point of this file: these are LOCAL.
  Dim As Integer p(0 To n - 1), pp(0 To n - 1), cnt(0 To n - 1)

  '' --- the permutation at index idxMin, straight from the index ---------------------------------
  '' Read idxMin in the factorial number system: its digit for position i is how far p(0..i) is
  '' rotated. cnt() keeps those digits, because the incremental step below is exactly "add one" in
  '' that same number system.
  For i As Integer = 0 To n - 1
    p(i) = i
    cnt(i) = 0
  Next i
  Dim As LongInt rest = idxMin
  For i As Integer = n - 1 To 1 Step -1
    Dim As Integer d = CInt(rest \ gFact(i))
    cnt(i) = d
    rest = rest Mod gFact(i)
    For j As Integer = 0 To i
      pp(j) = p(j)
    Next j
    For j As Integer = 0 To i
      If j + d <= i Then
        p(j) = pp(j + d)
      Else
        p(j) = pp(j + d - i - 1)
      End If
    Next j
  Next i

  '' --- walk this worker's range -----------------------------------------------------------------
  Dim As Integer maxflips = 0
  Dim As LongInt chk = 0
  Dim As LongInt idx = idxMin

  Do
    '' Count the flips of the current permutation. p(0) = 0 means it is already done: zero flips, and
    '' it contributes nothing to either the checksum or the maximum.
    If p(0) <> 0 Then
      Dim As Integer flips = 1
      Dim As Integer first = p(0)
      If p(first) <> 0 Then
        For j As Integer = 0 To n - 1
          pp(j) = p(j)
        Next j
        Do
          flips += 1
          '' Reverse pp(1 .. first-1); pp(0) and pp(first) are handled by the two lines after it,
          '' with pp(0)'s value living in `first` rather than in the array.
          Dim As Integer lo = 1, hi = first - 1
          Do While lo < hi
            Dim As Integer sw = pp(lo) : pp(lo) = pp(hi) : pp(hi) = sw
            lo += 1 : hi -= 1
          Loop
          Dim As Integer t = pp(first)
          pp(first) = first
          first = t
        Loop Until pp(first) = 0
      End If
      If flips > maxflips Then maxflips = flips
      '' The sign is the parity of the INDEX, which is what lets a worker score its own range
      '' without knowing how many permutations came before it.
      If (idx Mod 2) = 0 Then chk += flips Else chk -= flips
    End If

    idx += 1
    If idx >= idxMax Then Exit Do

    '' --- next permutation: "add one" in the factorial number system -------------------------------
    Dim As Integer first2 = p(1)
    p(1) = p(0)
    p(0) = first2
    Dim As Integer k = 1
    cnt(k) += 1
    Do While cnt(k) > k
      cnt(k) = 0
      k += 1
      Dim As Integer nxt = p(1)
      p(0) = nxt
      For j As Integer = 0 To k - 1
        p(j) = p(j + 1)
      Next j
      p(k) = first2
      first2 = nxt
      cnt(k) += 1
    Loop
  Loop

  gChk(id) = chk
  gMax(id) = maxflips
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 7
If Len(Command(1)) > 0 Then N = CInt(Command(1))
gN = N

gFact(0) = 1
For i As Integer = 1 To 20
  gFact(i) = gFact(i - 1) * i
Next i
gTotal = gFact(N)

'' One worker per LOGICAL PROCESSOR, per the project's threading default.
'' ⛔ ProcessorCount(), NOT CpuCount(): the three are different quantities and CpuCount() counts
'' SOCKETS, which is 1 on this machine. Using it made the default run single-threaded in silence -
'' the program still gave the right answer, just 8x slower, and the first timing read "1.30x".
gNW = ProcessorCount()
'' An optional SECOND argument pins the worker count. It is what separates "this shape costs more per
'' permutation" from "this shape does not scale": running it with 1 worker prices the algorithm against
'' the sequential version, with no threading in the answer at all.
If Len(Command(2)) > 0 Then gNW = CInt(Command(2))
If gNW < 1 Then gNW = 1
If gNW > 64 Then gNW = 64      '' the VM's live-worker ceiling
If CLngInt(gNW) > gTotal Then gNW = CInt(gTotal)
'' Ceiling division, so gNW chunks always cover the whole space.
gChunk = (gTotal + gNW - 1) \ gNW

ReDim gChk(0 To gNW - 1)
ReDim gMax(0 To gNW - 1)

Dim As Any Ptr h()
ReDim h(0 To gNW - 1)
For k As Integer = 0 To gNW - 1
  h(k) = ThreadCreate( @worker, k )
Next k
For k As Integer = 0 To gNW - 1
  ThreadWait( h(k) )
Next k

Dim As LongInt sum = 0
Dim As Integer maxflips = 0
For k As Integer = 0 To gNW - 1
  sum += gChk(k)
  If gMax(k) > maxflips Then maxflips = gMax(k)
Next k

Print Str(sum)
Print "Pfannkuchen("; Str(N); ") = "; Str(maxflips)
