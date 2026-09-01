'' ================================================================================================
''  BUDDHABROT - a real-time SedaiBasic demo
'' ================================================================================================
''
''  CREDIT, because the idea is not ours.
''    * Melinda Green discovered the technique and described it in 1993 on sci.fractals.
''    * Lori Gardi coined the name "Buddhabrot".
''    * Alexander Boswell later introduced Metropolis-Hastings sampling for deep zooms.
''  What is ours is this implementation. It was written from the mathematical description of the
''  algorithm and from nothing else: no existing Buddhabrot source was read while writing it.
''  See IMPLEMENTATIONS.md for how other languages express the same algorithm.
''
''  ----------------------------------------------------------------------------------------------
''  WHAT YOU ARE LOOKING AT
''  ----------------------------------------------------------------------------------------------
''  The Mandelbrot set is the set of complex numbers c for which the sequence
''
''        z(0) = 0 ,   z(n+1) = z(n)^2 + c
''
''  stays bounded forever. The usual Mandelbrot picture colours each c by how long its sequence
''  takes to run away. This picture asks a different question, and it is the whole idea:
''
''        not "which c escape?", but "WHERE DO THE ESCAPING ORBITS GO ON THEIR WAY OUT?"
''
''  So we throw a great many random c at the plane, and for every one whose orbit escapes we walk
''  back along the path that orbit took and add one to a counter at each pixel it passed through.
''  Points that never escape contribute nothing at all. The result is a density map - a photograph
''  of a probability cloud - and it does not look remotely like the Mandelbrot set. It looks like a
''  seated figure, which is where the name comes from.
''
''  Nothing here is an approximation of something prettier. The figure IS the density.
''
''  ----------------------------------------------------------------------------------------------
''  WHY THIS ALGORITHM WAS CHOSEN FOR A DEMO ABOUT EXECUTION SPEED
''  ----------------------------------------------------------------------------------------------
''  It is compute-bound and it has no shortcuts. There is no cache to exploit, no clever recurrence,
''  no early-out that skips real work: to get one more grain of signal you must trace one more orbit,
''  and tracing an orbit is arithmetic all the way down. A faster execution engine therefore buys
''  exactly one thing - more orbits per second - and that is a quantity you can SEE, because the
''  image converges out of noise while you watch it.
''
''  That is why every frame here gets the SAME amount of wall-clock time. The frame rate is held
''  steady on purpose and is not the measurement. What changes between the interpreter, the JIT and
''  the AOT compiler is how much of the picture appears in each of those equal slices.
''
''  ----------------------------------------------------------------------------------------------
''  THE FIVE THINGS THAT ARE EASY TO GET WRONG
''  ----------------------------------------------------------------------------------------------
''  Each of these is a place where a plausible-looking shortcut produces a wrong or ugly picture.
''  They are called out again at the point in the code where they matter.
''
''  1. YOU CANNOT ACCUMULATE WHILE YOU ITERATE. Half-way through an orbit you do not yet know
''     whether it escapes, and the orbits that do not escape must contribute nothing. So the orbit
''     is written to a scratch array first and replayed into the histogram only once escape is a
''     fact. Accumulating as you go silently draws the Mandelbrot set's interior into the picture.
''
''  2. TWO WHOLE REGIONS NEVER ESCAPE, AND THEY ARE CHEAP TO NAME. The main cardioid and the
''     period-2 bulb are the two big black areas of the Mandelbrot set. A sample landing in either
''     is guaranteed to iterate to the ceiling and then be thrown away. Two algebraic tests, about
''     ten floating-point operations, remove roughly one sample in six before any iteration at all.
''
''  3. THE FIGURE IS SYMMETRIC ABOUT THE REAL AXIS, SO EVERY ORBIT IS WORTH TWO. If c escapes then
''     so does its conjugate, along the mirrored path. Adding each orbit point twice - once as it
''     is, once mirrored - doubles the signal for the cost of one extra array write.
''
''  4. AN ITERATION FLOOR IS WHAT MAKES THE FIGURE APPEAR. Measured on this implementation, over
''     400 000 samples: 95% of escaping orbits are gone within eight iterations. Those short
''     orbits are barely more than the random point c itself, and together they lay down a flat grey
''     haze over the whole frame that no tone curve can remove, because it is real signal and it is
''     uniform. The structure lives in the rare long-lived orbits. Ignoring orbits that escape too
''     quickly is therefore not a cheat - it is choosing which question to photograph.
''
''  5. RAW COUNTS CANNOT BE SHOWN DIRECTLY. Their dynamic range is enormous, so a linear mapping
''     gives a black frame with a few burnt pixels. This uses a logarithmic curve.
''
''  ----------------------------------------------------------------------------------------------
''  DETERMINISM
''  ----------------------------------------------------------------------------------------------
''  The random source is a five-line xorshift generator written out below rather than the runtime's
''  RND. That is deliberate: the same seed must produce the same image on every execution engine and
''  on every machine, or the three-way comparison this demo exists for would be comparing three
''  different pictures. Verified: interpreter, JIT and AOT produce byte-identical output files.
''
''  ----------------------------------------------------------------------------------------------
''  RUNNING IT
''  ----------------------------------------------------------------------------------------------
''    sb --window bas/demo/buddhabrot/buddhabrot.bas
''    sb --window bas/demo/buddhabrot/buddhabrot.bas --jit   label=JIT
''    sb          bas/demo/buddhabrot/buddhabrot.bas still=4000000 out=/tmp/b.ppm
''
''  Arguments are name=value in any order; run with help=1 for the list.
''  Keys while it runs:  SPACE pause   R restart   S save a still   Q quit
'' ================================================================================================


'' ================================================================================================
''  1. THE VIEW
'' ================================================================================================
''  The escaping orbits all live inside the disc of radius 2, so that disc is what there is to
''  photograph. The window below is the part of it worth the pixels.
''
''  ⚠️ THE PICTURE IS TURNED A QUARTER TURN. The real axis runs DOWN the screen and the imaginary
''  axis runs ACROSS it. This is not a stylistic flourish and it is not free: it is what stands the
''  figure upright. Drawn the other way it lies on its side and reads as nothing in particular.

Const REAL_AXIS_MIN      = -2.0      '' maps to screen Y (top of the window)
Const REAL_AXIS_MAX      =  0.7      '' maps to screen Y (bottom)
Const IMAGINARY_AXIS_MIN = -1.35     '' maps to screen X (left)
Const IMAGINARY_AXIS_MAX =  1.35     '' maps to screen X (right)

''  Where the random points c are drawn from. Slightly wider than the view: an orbit that starts
''  just outside the window can still wander through it, and dropping those would shave the edges.
Const SAMPLE_REAL_MIN      = -2.2
Const SAMPLE_REAL_SPAN     =  3.0
Const SAMPLE_IMAGINARY_MIN = -1.4
Const SAMPLE_IMAGINARY_SPAN = 2.8

''  |z| > 2 means the orbit is gone: from there it grows without bound. Compare the SQUARED modulus
''  against 4 so the test never needs a square root - one multiply saved on the hottest line here.
Const ESCAPE_RADIUS_SQUARED = 4.0

Const ITERATION_CEILING = 20000      '' hard cap: sizes the scratch orbit array
Const TEXT_BAND_HEIGHT  = 24         '' pixels reserved at the top for the overlay
''  The equal slice of wall-clock time every frame gets, whatever engine is underneath.
''  ⚠️ 20 frames a second rather than 30, and the reason is a measured cost, not a preference.
''  Painting the window is one PSet per pixel, and PSet costs about 4 ns per call under the
''  interpreter and about the same under the JIT - but about 60 ns under AOT. (Measured flat across
''  100x100, 200x200 and 400x400, so it is a per-call cost and not a fixed overhead per frame.) A
''  full 400x400 repaint is therefore about 0.75 ms interpreted, 0.75 ms under the JIT and 9.5 ms
''  under AOT.
''  So the painting is a tax on the sampling budget, and - this is the awkward part - it is heaviest
''  on the FASTEST engine, which means this demo UNDERSTATES how much quicker AOT is. The shorter
''  the frame the worse that gets: at 33 ms the AOT engine spent about 30% of each frame painting
''  and traced 3.0x the interpreter's orbits; at 50 ms it spends about a fifth and the ratio moves
''  back towards the 4.1x the two engines actually differ by when nothing but arithmetic is timed.
''  Longer frames would be fairer still and would also look like a slideshow.
Const TARGET_FRAME_SECONDS = 0.050


'' ================================================================================================
''  2. THE RANDOM SOURCE
'' ================================================================================================
''  xorshift32, written out in full. Three shift-and-xor steps stir a 32-bit word; the masks keep it
''  to 32 bits because the state variable itself is 64-bit. It is not cryptographic and does not
''  need to be - it needs to be identical everywhere, and this is five lines of integer arithmetic
''  with no library and no platform behind it.

Dim Shared As LongInt randomState

Sub SeedRandom( ByVal seed As LongInt )
  '' Zero is the one state xorshift cannot leave, so it is quietly replaced.
  If seed = 0 Then seed = 2463534242
  randomState = seed And &hFFFFFFFF
End Sub

Function NextRandom() As Double        '' uniform in [0, 1)
  randomState = randomState Xor (randomState Shl 13) : randomState = randomState And &hFFFFFFFF
  randomState = randomState Xor (randomState Shr 17)
  randomState = randomState Xor (randomState Shl 5)  : randomState = randomState And &hFFFFFFFF
  Return randomState / 4294967296.0
End Function


'' ================================================================================================
''  3. THE TWO REGIONS THAT NEVER ESCAPE
'' ================================================================================================
''  Trap 2 from the header. Both tests are exact, not approximations.
''
''  The main cardioid is the big heart-shaped body of the Mandelbrot set. In polar form around its
''  cusp it satisfies  q * (q + (x - 1/4)) <= y^2 / 4  where  q = (x - 1/4)^2 + y^2.
''  The period-2 bulb is the disc of radius 1/4 centred at -1, so  (x + 1)^2 + y^2 <= 1/16.

Function PointIsProvablyInsideTheSet( ByVal cReal As Double, ByVal cImaginary As Double ) As Integer
  Dim As Double offsetFromCusp = cReal - 0.25
  Dim As Double q = offsetFromCusp * offsetFromCusp + cImaginary * cImaginary
  If q * (q + offsetFromCusp) <= 0.25 * cImaginary * cImaginary Then Return 1
  Dim As Double offsetFromBulb = cReal + 1.0
  If offsetFromBulb * offsetFromBulb + cImaginary * cImaginary <= 0.0625 Then Return 1
  Return 0
End Function


'' ================================================================================================
''  4. STATE
'' ================================================================================================

Dim Shared As Integer imageSize
Dim Shared As Integer minimumEscapeIterations
Dim Shared As Integer maximumIterations
Dim Shared As LongInt histogram()          '' one counter per pixel, laid out row by row
Dim Shared As Double  orbitReal(ITERATION_CEILING)
Dim Shared As Double  orbitImaginary(ITERATION_CEILING)
Dim Shared As Double  pixelsPerImaginaryUnit, pixelsPerRealUnit
Dim Shared As LongInt orbitsTraced, orbitsAccumulated


'' ================================================================================================
''  5. ACCUMULATION
'' ================================================================================================
''  One orbit point becomes one increment. Points that fall outside the window are dropped: the
''  orbit is free to wander anywhere in the disc of radius 2, and the window is smaller than that.

Sub AccumulateOrbitPoint( ByVal zReal As Double, ByVal zImaginary As Double )
  Dim As Integer column = Int( (zImaginary - IMAGINARY_AXIS_MIN) * pixelsPerImaginaryUnit )
  Dim As Integer row    = Int( (zReal      - REAL_AXIS_MIN)      * pixelsPerRealUnit )
  If column >= 0 And column < imageSize And row >= 0 And row < imageSize Then
    histogram(row * imageSize + column) = histogram(row * imageSize + column) + 1
  End If
End Sub


'' ================================================================================================
''  6. THE CENTRAL LOOP - one random point, one orbit, one decision
'' ================================================================================================
''  This is the whole algorithm. Everything above is preparation and everything below is presentation.

Sub TraceOneOrbit()
  orbitsTraced = orbitsTraced + 1

  '' A point drawn uniformly from the sampling rectangle.
  Dim As Double cReal      = SAMPLE_REAL_MIN      + NextRandom() * SAMPLE_REAL_SPAN
  Dim As Double cImaginary = SAMPLE_IMAGINARY_MIN + NextRandom() * SAMPLE_IMAGINARY_SPAN

  '' Trap 2: two algebraic tests remove about one sample in six before any iteration happens.
  If PointIsProvablyInsideTheSet(cReal, cImaginary) Then Exit Sub

  '' Iterate z <- z^2 + c from z = 0, writing the path down as we go.
  '' The squares are carried between iterations because each is needed twice: once to advance the
  '' orbit and once to test for escape. Recomputing them would double the multiplications here.
  Dim As Double zReal = 0.0, zImaginary = 0.0
  Dim As Double zRealSquared = 0.0, zImaginarySquared = 0.0
  Dim As Integer stepsTaken = 0

  Do While stepsTaken < maximumIterations _
           And zRealSquared + zImaginarySquared <= ESCAPE_RADIUS_SQUARED
    '' (a + bi)^2 = (a^2 - b^2) + 2abi.  The imaginary part is updated FIRST, because it needs the
    '' old zReal - swapping these two lines is the classic way to compute a different fractal.
    zImaginary = 2.0 * zReal * zImaginary + cImaginary
    zReal      = zRealSquared - zImaginarySquared + cReal
    zRealSquared      = zReal * zReal
    zImaginarySquared = zImaginary * zImaginary

    '' Trap 1: recorded, not accumulated. We still do not know how this ends.
    orbitReal(stepsTaken)      = zReal
    orbitImaginary(stepsTaken) = zImaginary
    stepsTaken = stepsTaken + 1
  Loop

  '' Did it get out, and did it take long enough to be interesting? (Traps 1 and 4.)
  If zRealSquared + zImaginarySquared <= ESCAPE_RADIUS_SQUARED Then Exit Sub   '' still bounded
  If stepsTaken < minimumEscapeIterations Then Exit Sub                        '' escaped too fast

  '' Now replay the path into the picture. Trap 3: each point counts twice, once mirrored, because
  '' the conjugate of an escaping point escapes along the mirrored path.
  orbitsAccumulated = orbitsAccumulated + 1
  Dim As Integer i
  For i = 0 To stepsTaken - 1
    AccumulateOrbitPoint( orbitReal(i),  orbitImaginary(i) )
    AccumulateOrbitPoint( orbitReal(i), -orbitImaginary(i) )
  Next i
End Sub


'' ================================================================================================
''  7. TONE MAPPING
'' ================================================================================================
''  Trap 5. Counts span roughly three orders of magnitude across one frame, so a logarithm is what
''  turns them into something an eye can read. The palette is a warm ramp: dark red through orange
''  into white, which is a reasonable convention for a density map and keeps the faint outer
''  filaments visible instead of crushing them to black.

Dim Shared As Double toneGamma

Function ClampUnit( ByVal v As Double ) As Double
  If v <= 0.0 Then Return 0.0
  If v >= 1.0 Then Return 1.0
  Return v
End Function

Function BrightnessOf( ByVal count As LongInt, ByVal inverseLogOfPeak As Double ) As Double
  If count <= 0 Then Return 0.0
  Return (Log(1.0 + count) * inverseLogOfPeak) ^ toneGamma
End Function

'' A fire ramp in three equal thirds: black to red, red to orange, orange to white. Each channel is
'' the same straight line shifted by a third, which is why it reads as one continuous heat scale
'' rather than three colours meeting at seams.
Function ColourFor( ByVal brightness As Double ) As Integer
  If brightness <= 0.0 Then Return RGB(0, 0, 0)
  Dim As Integer red   = Int( 255.0 * ClampUnit( brightness * 3.0 ) )
  Dim As Integer green = Int( 255.0 * ClampUnit( (brightness - 0.3333) * 3.0 ) )
  Dim As Integer blue  = Int( 255.0 * ClampUnit( (brightness - 0.6667) * 3.0 ) )
  Return RGB(red, green, blue)
End Function

''  ⚠️ THE COLOUR IS COMPUTED PER DISTINCT COUNT, NOT PER PIXEL, and that is a load-bearing choice
''  rather than a micro-optimisation. Colouring pixel by pixel costs one logarithm per PIXEL, which
''  is 160 000 of them at 400x400 - measured, that alone was larger than the whole frame budget, and
''  it hurt the compiled engines worst, because 160 000 logarithms is also 160 000 calls. Building a
''  table indexed by the count costs one logarithm per DISTINCT COUNT, a few thousand at most.
''  Same arithmetic, same picture, two orders of magnitude less of it.
''  The screen and the exported file both read this table, so the two cannot disagree.

Dim Shared As Integer colourOfCount()

Sub RebuildColourTable( ByVal peak As LongInt )
  If peak < 1 Then peak = 1
  If UBound(colourOfCount) < peak Then ReDim colourOfCount(peak + peak \ 2)
  Dim As Double inverseLogOfPeak = 1.0 / Log(1.0 + peak)
  colourOfCount(0) = RGB(0, 0, 0)
  Dim As LongInt count
  For count = 1 To peak
    colourOfCount(count) = ColourFor( BrightnessOf(count, inverseLogOfPeak) )
  Next count
End Sub

Function PeakCount() As LongInt
  Dim As LongInt peak = 0
  Dim As Integer i
  For i = 0 To imageSize * imageSize - 1
    If histogram(i) > peak Then peak = histogram(i)
  Next i
  Return peak
End Function


'' ================================================================================================
''  8. OUTPUT
'' ================================================================================================
''  A binary PPM (P6): a nine-byte header and then three bytes per pixel. No compression, no
''  library, and every viewer on earth reads it. It is also what the determinism check hashes -
''  the file is written from the HISTOGRAM, not read back off the screen, so the comparison between
''  execution engines is a comparison of the arithmetic and nothing else.

Sub WritePortablePixmap( ByVal fileName As String )
  RebuildColourTable( PeakCount() )

  Dim As Integer handle = FreeFile
  Open fileName For Binary Access Write As #handle
  Put #handle, , "P6" + Chr(10) + Str(imageSize) + " " + Str(imageSize) + Chr(10) + "255" + Chr(10)

  Dim As String row
  Dim As Integer x, y
  For y = 0 To imageSize - 1
    row = ""
    For x = 0 To imageSize - 1
      Dim As Integer colour = colourOfCount( histogram(y * imageSize + x) )
      row += Chr((colour Shr 16) And 255) + Chr((colour Shr 8) And 255) + Chr(colour And 255)
    Next x
    Put #handle, , row
  Next y
  Close #handle
End Sub

''  ⛔⛔ EVERYTHING THAT BELONGS TO ONE FRAME HAPPENS INSIDE ONE LOCK, the overlay included.
''  While a lock is held nothing is presented; the unlock presents once. Drawing the text after the
''  unlock - which is where it was first written - presents the frame THREE times, once for the
''  image and once for each line of text, and the picture and the numbers written over it are never
''  shown as the same instant.
''  The band is cleared before it is written, because a number that gets shorter would otherwise
''  leave the tail of the longer one it replaced standing behind it.

Sub PaintFrame( ByVal topLine As String, ByVal bottomLine As String )
  RebuildColourTable( PeakCount() )
  Dim As Integer x, y
  ScreenLock
  For y = 0 To imageSize - 1
    For x = 0 To imageSize - 1
      PSet (x, y + TEXT_BAND_HEIGHT), colourOfCount( histogram(y * imageSize + x) )
    Next x
  Next y
  Line (0, 0)-(imageSize - 1, TEXT_BAND_HEIGHT - 1), RGB(0, 0, 0), BF
  Draw String (4, 2),  topLine,    RGB(255, 255, 255)
  Draw String (4, 12), bottomLine, RGB(160, 160, 160)
  ScreenUnlock
End Sub


'' ================================================================================================
''  9. ARGUMENTS
'' ================================================================================================

Function ArgumentValue( ByVal name As String, ByVal fallback As String ) As String
  Dim As Integer i = 1
  Do While Len(Command(i)) > 0
    Dim As String argument = Command(i)
    If Left(argument, Len(name) + 1) = name + "=" Then
      Return Mid(argument, Len(name) + 2)
    End If
    i = i + 1
  Loop
  Return fallback
End Function

Sub PrintUsage()
  Print "buddhabrot.bas - name=value arguments, any order"
  Print
  Print "  seed=N      random seed                            (default 2463534242)"
  Print "  size=N      image is N by N pixels                 (default 400)"
  Print "  min=N       ignore orbits escaping in under N steps (default 20)"
  Print "  max=N       iteration ceiling                      (default 2000)"
  Print "  label=TEXT  name shown in the overlay              (default ENGINE)"
  Print "  secs=N      live mode: stop after N seconds        (default 0 = until Q)"
  Print "  still=N     compute N orbits, write a file, exit   (no window)"
  Print "  out=FILE    where still= writes                    (default buddhabrot.ppm)"
  Print "  gamma=N     tone curve applied after the log        (default 2.2)"
  Print
  Print "Keys while running:  SPACE pause   R restart   S save a still   Q quit"
End Sub


'' ================================================================================================
''  10. MAIN
'' ================================================================================================

If ArgumentValue("help", "") <> "" Then
  PrintUsage()
  End
End If

imageSize               = CInt( ArgumentValue("size", "400") )
minimumEscapeIterations = CInt( ArgumentValue("min",  "20") )
maximumIterations       = CInt( ArgumentValue("max",  "2000") )
Dim As LongInt seed     = CLngInt( ArgumentValue("seed", "2463534242") )
Dim As String  label    = ArgumentValue("label", "ENGINE")
Dim As LongInt stillOrbits = CLngInt( ArgumentValue("still", "0") )
Dim As String  outputName  = ArgumentValue("out", "buddhabrot.ppm")
Dim As Double  runSeconds  = CDbl( ArgumentValue("secs", "0") )
toneGamma = CDbl( ArgumentValue("gamma", "2.2") )

If maximumIterations > ITERATION_CEILING Then maximumIterations = ITERATION_CEILING

ReDim histogram(imageSize * imageSize - 1)
pixelsPerImaginaryUnit = imageSize / (IMAGINARY_AXIS_MAX - IMAGINARY_AXIS_MIN)
pixelsPerRealUnit      = imageSize / (REAL_AXIS_MAX - REAL_AXIS_MIN)
ReDim colourOfCount(4095)
SeedRandom(seed)
orbitsTraced = 0
orbitsAccumulated = 0

'' ---- still mode: no window, fixed work, one file. This is what the determinism check runs. -----
If stillOrbits > 0 Then
  Dim As Double startedAt = Timer
  Do While orbitsTraced < stillOrbits
    TraceOneOrbit()
  Loop
  Dim As Double took = Timer - startedAt
  WritePortablePixmap(outputName)
  Print "orbits traced      : "; orbitsTraced
  Print "orbits accumulated : "; orbitsAccumulated
  Print "peak pixel count   : "; PeakCount()
  Print "seconds            : "; took
  Print "orbits per second  : "; Int(orbitsTraced / took)
  Print "written            : "; outputName
  End
End If

'' ---- live mode ---------------------------------------------------------------------------------
ScreenRes imageSize, imageSize + TEXT_BAND_HEIGHT, 32
WindowTitle "SedaiBasic - Buddhabrot - " + label

'' Every frame gets the same slice of wall-clock time, and the slice has to cover BOTH halves of the
'' frame: the orbits AND the painting. Sampling for a fixed time and then painting on top of it
'' would make the frame rate depend on how fast the painting is, which differs between engines - and
'' the one thing this demo must not do is let the frame rate become the measurement.
Dim As Double paintSeconds = 0.010
Dim As Double startedAt = Timer
Dim As Double orbitsPerSecond = 0.0
Dim As Integer paused = 0
Dim As LongInt framesDrawn = 0
Dim As String  key

Do
  Dim As Double frameStart = Timer
  framesDrawn = framesDrawn + 1

  If paused = 0 Then
    Dim As Double sampleBudget = TARGET_FRAME_SECONDS - paintSeconds
    If sampleBudget < 0.002 Then sampleBudget = 0.002
    Dim As Double sampleStart = Timer
    Dim As LongInt before = orbitsTraced
    '' Timer is read once per batch, not once per orbit: at these speeds the clock call would
    '' otherwise be a measurable share of the work it is timing.
    Do
      Dim As Integer batch
      For batch = 1 To 256
        TraceOneOrbit()
      Next batch
    Loop Until Timer - sampleStart >= sampleBudget
    Dim As Double sampleSeconds = Timer - sampleStart
    If sampleSeconds > 0.0 Then orbitsPerSecond = (orbitsTraced - before) / sampleSeconds
  End If

  '' Draw String writes into the framebuffer and nowhere else. LOCATE + PRINT would also work, but
  '' PRINT echoes to standard output as well, and a demo that scrolls a thousand lines up the
  '' terminal it was launched from is not one anybody runs twice.
  Dim As Double paintStart = Timer
  PaintFrame( label + "   " + Str(Int(orbitsPerSecond)) + " orbits/s" + _
                IIf(paused, "   [PAUSED]", ""), _
              "traced " + Str(orbitsTraced) + "    drawn " + Str(orbitsAccumulated) + _
                "    iter " + Str(maximumIterations) )
  paintSeconds = Timer - paintStart

  key = LCase(InKey)
  If key = "q" Or key = Chr(27) Then Exit Do
  If key = " " Then paused = 1 - paused
  If key = "r" Then
    Dim As Integer i
    For i = 0 To imageSize * imageSize - 1 : histogram(i) = 0 : Next i
    SeedRandom(seed) : orbitsTraced = 0 : orbitsAccumulated = 0 : startedAt = Timer
  End If
  If key = "s" Then WritePortablePixmap(outputName)
  If key = "+" Then maximumIterations = maximumIterations * 2
  If key = "-" And maximumIterations > 50 Then maximumIterations = maximumIterations \ 2
  If maximumIterations > ITERATION_CEILING Then maximumIterations = ITERATION_CEILING

  If runSeconds > 0.0 And Timer - startedAt >= runSeconds Then Exit Do
Loop

WritePortablePixmap(outputName)
Print
Dim As Double ranFor = Timer - startedAt
Print "orbits traced     : "; orbitsTraced
Print "orbits per second : "; Int(orbitsPerSecond)
'' The frame rate is the thing this demo holds CONSTANT, so it is worth printing: it should come out
'' near the same number on every engine, while the orbit count above should not.
Print "frames drawn      : "; framesDrawn
If ranFor > 0.0 Then Print "frames per second : "; Int(framesDrawn / ranFor)
Print "written           : "; outputName


'' ================================================================================================
''  IMPROVEMENTS DELIBERATELY LEFT OUT, because each one costs more clarity than it is worth here
'' ================================================================================================
''
''  * REPAINT ONLY WHAT CHANGED. PaintFrame writes every pixel every frame. Keeping a shadow copy of
''    the last colour drawn and skipping unchanged pixels would cut most of that, and it matters
''    more than it looks: a full 400x400 repaint costs about 0.75 ms under the interpreter and the
''    JIT but about 9.5 ms under AOT, which is a fifth of a 50 ms frame. It needs a second full-size
''    buffer and a comparison in the inner loop.
''
''  * INCREMENTAL PEAK. PeakCount rescans the whole histogram once per frame to find the brightest
''    pixel. The peak only ever grows, so it could be maintained as orbits are accumulated. Left
''    out because the rescan is a plain loop anyone can read and the saving is under a millisecond.
''
''  * METROPOLIS-HASTINGS SAMPLING. Instead of drawing c uniformly, mutate a c that is known to
''    produce a long orbit and accept or reject the mutation. It converges far faster at deep zoom.
''    It is a different algorithm with a different convergence behaviour, so mixing it in would make
''    the comparison in IMPLEMENTATIONS.md meaningless. It belongs in a separate variant.
''
''  * MORE THAN ONE THREAD. Each orbit is independent, so this parallelises almost perfectly. It is
''    left out on purpose: three engines compared through a thread pool measure the thread pool.
''
''  * THE NEBULA COLOURING. Run three iteration windows into three histograms and map them to red,
''    green and blue. It is only three more histograms and it is the version most people picture -
''    but it triples the memory and the paint cost for no gain in what this demo is showing.
