'' The Computer Language Benchmarks Game
'' https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
''
'' mandelbrot, SedaiBasic MODERN dialect.
'' Ported from the Python version (contributed by Joerg Baumann).
''
'' Parallel, like the Python original: that one hands one ROW per job to a Pool, so this one gives
'' each of four workers a contiguous band of rows. Same weapons.
''
'' Output is a binary PBM (P4): one bit per pixel, 8 pixels to a byte, MSB first. Rows are computed
'' into a shared byte array and written out IN ORDER by the main thread, which is what Python's
'' ordered_rows does after imap_unordered.

Dim Shared As Integer NW    '' workers - resolved from the machine below

Dim Shared As Integer nSize, bytesPerRow
Dim Shared As Any Ptr mtx, cvWork, cvDone
Dim Shared As Integer gPhase, gDone, gQuit
Dim Shared As Integer bits()        '' nSize * bytesPerRow bytes, one row after another

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

    Dim As Double c1 = 2.0 / nSize
    Dim As Integer span = nSize \ NW
    Dim As Integer y0 = id * span
    Dim As Integer y1 = y0 + span - 1
    If id = NW - 1 Then y1 = nSize - 1

    For y As Integer = y0 To y1
      Dim As Double ci = y * c1 - 1.0
      Dim As Integer rowBase = y * bytesPerRow
      For bx As Integer = 0 To bytesPerRow - 1
        Dim As Integer pixel = 0
        Dim As Integer bit = 128
        '' The original walks the eight pixels of a byte by ADDING c1 each time, rather than by
        '' multiplying the pixel index. The two are the same in arithmetic and NOT the same in floating
        '' point: computing it the other way flipped exactly one pixel of the whole picture, right on
        '' the edge of the set. Accumulate, like the reference does.
        Dim As Double cr = (bx * 8) * c1 - 1.5
        For k As Integer = 0 To 7
          Dim As Double zr = cr, zi = ci
          Dim As Integer inside = 1
          '' 7 groups of 7 iterations, magnitude tested between groups - exactly as the original
          For g As Integer = 1 To 7
            For h As Integer = 1 To 7
              Dim As Double t = zr * zr - zi * zi + cr
              zi = 2.0 * zr * zi + ci
              zr = t
            Next h
            If zr * zr + zi * zi >= 4.0 Then
              inside = 0
              Exit For
            End If
          Next g
          If inside <> 0 Then pixel += bit
          bit \= 2
          cr += c1
        Next k
        bits(rowBase + bx) = pixel
      Next bx
    Next y

    MutexLock mtx
    gDone += 1
    CondBroadcast cvDone
    MutexUnlock mtx
  Loop
End Sub

'' N comes from the command line, as in every reference implementation; the literal is the fallback.
Dim As Integer N = 200
If Len(Command(1)) > 0 Then N = CInt(Command(1))
'' Workers: as many as the machine's LOGICAL processors, because that is what the Python original
'' asks for - Pool() with no argument is cpu_count(). Sizing this to a hardcoded 4 is what made our
'' lead collapse when the machine went from 4 cores to 16: Python took the new cores, we did not.
'' An explicit SECOND command-line argument overrides it, for measuring at a fixed width.
'' ⛔ PROCESSORCOUNT is a MODERN extension (fbc has no equivalent) - see BASIC.md.
NW = ProcessorCount
If Len(Command(2)) > 0 Then NW = CInt(Command(2))
If NW < 1 Then NW = 1


nSize = N
bytesPerRow = (N + 7) \ 8
ReDim bits(0 To nSize * bytesPerRow - 1)

mtx = MutexCreate() : cvWork = CondCreate() : cvDone = CondCreate()
gPhase = 0 : gDone = 0 : gQuit = 0
Dim As Any Ptr h()
ReDim h(0 To NW - 1)
For k As Integer = 0 To NW - 1
  h(k) = ThreadCreate( @worker, k )
Next k

'' one phase: the whole picture
MutexLock mtx
gDone = 0
gPhase += 1
CondBroadcast cvWork
Do While gDone < NW
  CondWait cvDone, mtx
Loop
MutexUnlock mtx

MutexLock mtx
gQuit = 1
CondBroadcast cvWork
MutexUnlock mtx
For k As Integer = 0 To NW - 1
  ThreadWait( h(k) )
Next k
MutexDestroy mtx : CondDestroy cvWork : CondDestroy cvDone

'' Mask off the bits past the right edge in the last byte of each row: "result[-1] &= 0xff << (8 - n % 8)".
'' Replicated LITERALLY, including the case the original probably did not intend: when n is a multiple
'' of 8 the shift is 8, the mask becomes 0x100, and a byte ANDed with that is ZERO - so the whole last
'' byte of every row is cleared. That is what the reference implementation outputs, so it is what a
'' comparable implementation has to output too.
Dim As Integer m = (255 Shl (8 - (N Mod 8))) And 255
For y As Integer = 0 To N - 1
  Dim As Integer idx = y * bytesPerRow + bytesPerRow - 1
  bits(idx) = bits(idx) And m
Next y

Print "P4"; Chr(10);
Print Str(N); " "; Str(N); Chr(10);
For y As Integer = 0 To N - 1
  Dim As String row = ""
  Dim As Integer rowBase = y * bytesPerRow
  For bx As Integer = 0 To bytesPerRow - 1
    row += Chr(bits(rowBase + bx))
  Next bx
  Print row;
Next y
