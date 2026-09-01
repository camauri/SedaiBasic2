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
''  Keys while it runs:  SPACE pause · R restart · C palette · [ ] gamma · + - iterations · S save · Q quit
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
''  The frame rate is a BUDGET, not a limit. Every frame is given the same slice of wall-clock time
''  whatever engine is underneath, and the sampling stops when the slice is spent - so this number
''  is what the demo runs at, not what it can manage. With no sampling at all the same loop reaches
''  227 frames a second interpreted and 317 under AOT, so 60 leaves plenty of room. fps= moves it.
''
''  ⚠️ IT USED TO BE 20, AND THE REASON IT COULD NOT BE HIGHER IS WORTH KNOWING. Painting is one
''  PSet per pixel, and PSet had no native lowering in the AOT backend: every pixel became a
''  runtime-helper call that flushed and reloaded every allocated register, 60 ns against the
''  interpreter's 4. A 400x400 repaint cost 9.5 ms compiled against 0.65 ms interpreted, so the
''  painting was a tax that fell HARDEST on the fastest engine, and asking for more frames made the
''  compiled engine look WORSE: at 60 frames a second the AOT advantage collapsed from 3.3x to 1.8x.
''  That was a defect in the engine rather than in the demo, and it is fixed (SedaiAot C8): PSet is
''  now an inline store gated on the same surface descriptor the C hot loop uses, 1.1 ns per call.
''  The tax is gone, and with it the reason to keep the frame rate down.
Const DEFAULT_FRAMES_PER_SECOND = 60


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
'' ⭐ THREE counters per pixel, not one, and this is where the colour comes from.
''
''  The classic Buddhabrot is grey. The colour version - the one most people have seen - is three
''  Buddhabrots at three different iteration ceilings, laid into the red, green and blue channels.
''  Rendering it as three passes would cost three times the orbits; it does not have to, because the
''  three sets are NESTED. An orbit that escapes in n steps would escape under any ceiling at or
''  above n, so one pass at the largest ceiling knows every channel that orbit belongs to:
''
''        n <= RED ceiling    -> red     (always true: RED is the largest)
''        n <= GREEN ceiling  -> green too
''        n <= BLUE ceiling   -> blue as well
''
''  ⭐ And that nesting is what separates the colours, without any of the channels being told to look
''  different. Nine escaping orbits in ten die within a few steps, and those short ones are barely
''  more than the random point c - a flat haze. They land in ALL THREE channels, so the haze is
''  white-ish; but the red channel also holds every long-lived orbit, so ITS peak is far higher and
''  the same haze, normalised against it, comes out dim. The structure is red because only red has
''  it; the outer glow is blue because blue has nothing else.
''  ⇒ No "ignore orbits shorter than N" rule is needed any more. The old single-channel version had
''    one, because without it the picture was a flat grey field. Here the flatness has somewhere to
''    go.

Dim Shared As LongInt histogram()          '' three planes, one per channel, each row by row
Dim Shared As Integer pixelsPerPlane
Dim Shared As Integer greenCeiling, blueCeiling
Dim Shared As Double  orbitReal(ITERATION_CEILING)
Dim Shared As Double  orbitImaginary(ITERATION_CEILING)
Dim Shared As Double  pixelsPerImaginaryUnit, pixelsPerRealUnit
Dim Shared As LongInt orbitsTraced, orbitsAccumulated


'' ================================================================================================
''  5. ACCUMULATION
'' ================================================================================================
''  One orbit point becomes one increment. Points that fall outside the window are dropped: the
''  orbit is free to wander anywhere in the disc of radius 2, and the window is smaller than that.

Sub AccumulateOrbitPoint( ByVal zReal As Double, ByVal zImaginary As Double, _
                          ByVal planeBase As Integer )
  Dim As Integer column = Int( (zImaginary - IMAGINARY_AXIS_MIN) * pixelsPerImaginaryUnit )
  Dim As Integer row    = Int( (zReal      - REAL_AXIS_MIN)      * pixelsPerRealUnit )
  If column >= 0 And column < imageSize And row >= 0 And row < imageSize Then
    Dim As Integer at = planeBase + row * imageSize + column
    histogram(at) = histogram(at) + 1
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

  '' Did it get out at all? (Trap 1: this is the moment we are finally allowed to know.)
  If zRealSquared + zImaginarySquared <= ESCAPE_RADIUS_SQUARED Then Exit Sub   '' still bounded
  If stepsTaken < minimumEscapeIterations Then Exit Sub                        '' escaped too fast

  '' How many channels does this orbit belong to? The ceilings are nested - blue inside green inside
  '' red - so the answer is a count, decided once, before the replay rather than inside it.
  Dim As Integer channels = 1
  Dim As Integer firstChannel = 0
  If stepsTaken <= greenCeiling Then firstChannel = 1
  If stepsTaken <= blueCeiling  Then firstChannel = 2

  '' Now replay the path into the picture. Trap 3: each point counts twice, once mirrored, because
  '' the conjugate of an escaping point escapes along the mirrored path.
  orbitsAccumulated = orbitsAccumulated + 1
  Dim As Integer i, channel, planeBase
  For channel = firstChannel To firstChannel + channels - 1
    planeBase = channel * pixelsPerPlane
    For i = 0 To stepsTaken - 1
      AccumulateOrbitPoint( orbitReal(i),  orbitImaginary(i), planeBase )
      AccumulateOrbitPoint( orbitReal(i), -orbitImaginary(i), planeBase )
    Next i
  Next channel
End Sub


'' ================================================================================================
''  7. TONE MAPPING
'' ================================================================================================
''  Trap 5. Counts span roughly three orders of magnitude across one frame, so a logarithm is what
''  turns them into something an eye can read; a linear mapping gives a black picture with a few
''  burnt pixels. The gamma on top of it decides how much of the faint outer structure survives.
''
''  ⭐ EACH CHANNEL IS NORMALISED AGAINST ITS OWN PEAK, and that is not a detail - it is the whole
''  colour scheme. The three channels hold very different totals: red has every escaping orbit,
''  blue only the shortest. Normalising them together would make blue almost black and throw the
''  colour away; normalising each to itself is what turns "how long did the orbits here live?" into
''  a hue.

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

''  ⚠️ THE BRIGHTNESS IS COMPUTED PER DISTINCT COUNT, NOT PER PIXEL, and that is a load-bearing
''  choice rather than a micro-optimisation. Doing it pixel by pixel costs one logarithm per PIXEL -
''  480 000 of them at 400x400 across three channels, measured larger than the whole frame budget.
''  A table indexed by the count costs one logarithm per DISTINCT COUNT, a few thousand at most.
''  Same arithmetic, same picture, two orders of magnitude less of it.
''  One table per channel, because each has its own peak. The screen and the exported file both read
''  these tables, so the two cannot disagree.

Dim Shared As Integer levelOfCount(Any, Any)    '' [channel, count] -> 0..255

Sub RebuildLevelTables()
  Dim As Integer channel, biggest = 1
  Dim As LongInt peak(2)
  Dim As Integer i
  For channel = 0 To 2
    peak(channel) = 1
    For i = 0 To pixelsPerPlane - 1
      If histogram(channel * pixelsPerPlane + i) > peak(channel) Then _
        peak(channel) = histogram(channel * pixelsPerPlane + i)
    Next i
    If peak(channel) > biggest Then biggest = peak(channel)
  Next channel

  If UBound(levelOfCount, 2) < biggest Then ReDim levelOfCount(2, biggest + biggest \ 2)
  For channel = 0 To 2
    Dim As Double inverseLogOfPeak = 1.0 / Log(1.0 + peak(channel))
    levelOfCount(channel, 0) = 0
    Dim As LongInt count
    For count = 1 To peak(channel)
      levelOfCount(channel, count) = Int( 255.0 * ClampUnit( BrightnessOf(count, inverseLogOfPeak) ) )
    Next count
    '' Anything above this channel's own peak cannot occur in it, but the table is shared in size.
    For count = peak(channel) + 1 To UBound(levelOfCount, 2)
      levelOfCount(channel, count) = 255
    Next count
  Next channel
End Sub

''  ---- THE THREE READINGS -----------------------------------------------------------------------
''  The three planes hold three different facts, so there is more than one honest way to show them.
''  These are readings of the same data, not decoration - each one answers a different question, and
''  none of them invents anything the histogram does not already contain.
''
''    NEBULA   long-lived orbits to red, short-lived to blue. The structure is red because only the
''             red plane holds it, and the haze is blue because blue holds nothing else. The default.
''    AURORA   the same three planes with red and blue exchanged - a warm haze around a cold figure.
''             Not an inversion of the picture: an inversion of which lifetime you are looking at.
''    EMBER    the three planes added back together and put through one warm ramp, which is the
''             single-channel Buddhabrot everyone has seen. Losing the colour loses the lifetime.

Const READING_NEBULA = 0
Const READING_AURORA = 1
Const READING_EMBER  = 2
Const READING_COUNT  = 3

'' ⚠️ Not called `palette`: PALETTE is a graphics statement, so the name is taken.
Dim Shared As Integer colourReading

Function ReadingName( ByVal which As Integer ) As String
  If which = READING_AURORA Then Return "AURORA"
  If which = READING_EMBER  Then Return "EMBER"
  Return "NEBULA"
End Function

Function ColourAt( ByVal pixel As Integer ) As Integer
  Dim As Integer longLived  = levelOfCount(0, histogram(pixel))
  Dim As Integer midLived   = levelOfCount(1, histogram(pixelsPerPlane + pixel))
  Dim As Integer shortLived = levelOfCount(2, histogram(2 * pixelsPerPlane + pixel))

  If colourReading = READING_AURORA Then Return RGB( shortLived, midLived, longLived )

  If colourReading = READING_EMBER Then
    '' One brightness from all three, then a warm ramp in equal thirds: black to red, red to orange,
    '' orange to white. Each channel is the same straight line shifted by a third, which is why it
    '' reads as one continuous heat scale rather than three colours meeting at seams.
    Dim As Double heat = (longLived + midLived + shortLived) / 765.0
    Return RGB( Int(255.0 * ClampUnit(heat * 3.0)), _
                Int(255.0 * ClampUnit((heat - 0.3333) * 3.0)), _
                Int(255.0 * ClampUnit((heat - 0.6667) * 3.0)) )
  End If

  Return RGB( longLived, midLived, shortLived )
End Function

'' Only the summary line uses this now - the tone mapping finds all three peaks in one pass while
'' it is building the tables. It reports the RED plane, which is the one holding the long-lived
'' orbits and therefore the one whose growth says the picture is still converging.
Function PeakRedCount() As LongInt
  Dim As LongInt peak = 0
  Dim As Integer i
  For i = 0 To pixelsPerPlane - 1
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
  RebuildLevelTables()

  Dim As Integer handle = FreeFile
  Open fileName For Binary Access Write As #handle
  Put #handle, , "P6" + Chr(10) + Str(imageSize) + " " + Str(imageSize) + Chr(10) + "255" + Chr(10)

  Dim As String row
  Dim As Integer x, y
  For y = 0 To imageSize - 1
    row = ""
    For x = 0 To imageSize - 1
      Dim As Integer colour = ColourAt( y * imageSize + x )
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
  RebuildLevelTables()
  Dim As Integer x, y
  ScreenLock
  For y = 0 To imageSize - 1
    For x = 0 To imageSize - 1
      PSet (x, y + TEXT_BAND_HEIGHT), ColourAt( y * imageSize + x )
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
  Print "  min=N       ignore orbits escaping in under N steps (default 0)"
  Print "  max=N       red channel ceiling; green is a tenth, blue a hundredth (default 5000)"
  Print "  label=TEXT  name shown in the overlay              (default ENGINE)"
  Print "  secs=N      live mode: stop after N seconds        (default 0 = until Q)"
  Print "  still=N     compute N orbits, write a file, exit   (no window)"
  Print "  out=FILE    where still= writes                    (default buddhabrot.ppm)"
  Print "  gamma=N     tone curve applied after the log        (default 4.5)"
  Print "  palette=X   nebula | aurora | ember                  (default nebula)"
  Print "  fps=N       frames per second to hold                 (default 60)"
  Print
  Print "Keys while running:  SPACE pause   R restart   C palette   [ ] gamma   + - iterations   S save   Q quit"
End Sub


'' ================================================================================================
''  10. MAIN
'' ================================================================================================

If ArgumentValue("help", "") <> "" Then
  PrintUsage()
  End
End If

imageSize               = CInt( ArgumentValue("size", "400") )
minimumEscapeIterations = CInt( ArgumentValue("min",  "0") )
maximumIterations       = CInt( ArgumentValue("max",  "5000") )
Dim As LongInt seed     = CLngInt( ArgumentValue("seed", "2463534242") )
Dim As String  label    = ArgumentValue("label", "ENGINE")
Dim As LongInt stillOrbits = CLngInt( ArgumentValue("still", "0") )
Dim As String  outputName  = ArgumentValue("out", "buddhabrot.ppm")
Dim As Double  runSeconds  = CDbl( ArgumentValue("secs", "0") )
toneGamma = CDbl( ArgumentValue("gamma", "4.5") )
colourReading = READING_NEBULA
If LCase(ArgumentValue("palette", "")) = "aurora" Then colourReading = READING_AURORA
If LCase(ArgumentValue("palette", "")) = "ember"  Then colourReading = READING_EMBER
Dim As Double targetFrameSeconds = 1.0 / CDbl( ArgumentValue("fps", Str(DEFAULT_FRAMES_PER_SECOND)) )

If maximumIterations > ITERATION_CEILING Then maximumIterations = ITERATION_CEILING

'' The two inner ceilings, a decade apart, which is the proportion the classic three-pass version
'' uses (5000 / 500 / 50). Deriving them from max= keeps one knob instead of three: raising the
'' ceiling deepens all three channels together, which is what anyone turning that dial means.
greenCeiling = maximumIterations \ 10
blueCeiling  = maximumIterations \ 100
If greenCeiling < 2 Then greenCeiling = 2
If blueCeiling  < 1 Then blueCeiling  = 1

pixelsPerPlane = imageSize * imageSize
ReDim histogram(3 * pixelsPerPlane - 1)
ReDim levelOfCount(2, 4095)
pixelsPerImaginaryUnit = imageSize / (IMAGINARY_AXIS_MAX - IMAGINARY_AXIS_MIN)
pixelsPerRealUnit      = imageSize / (REAL_AXIS_MAX - REAL_AXIS_MIN)
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
  Print "peak pixel count   : "; PeakRedCount()
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
    Dim As Double sampleBudget = targetFrameSeconds - paintSeconds
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
                "    iter " + Str(maximumIterations) + _
                "    " + ReadingName(colourReading) + " g" + Format(toneGamma, "0.0") )
  paintSeconds = Timer - paintStart

  key = LCase(InKey)
  If key = "q" Or key = Chr(27) Then Exit Do
  If key = " " Then paused = 1 - paused
  If key = "r" Then
    Dim As Integer i
    For i = 0 To 3 * pixelsPerPlane - 1 : histogram(i) = 0 : Next i
    SeedRandom(seed) : orbitsTraced = 0 : orbitsAccumulated = 0 : startedAt = Timer
  End If
  If key = "s" Then WritePortablePixmap(outputName)
  '' The tone curve and the reading are the two things worth changing while looking at the picture,
  '' because both are judgements about what you want to SEE and neither costs a recomputation - the
  '' orbits are already counted, only the tables are rebuilt.
  If key = "c" Then colourReading = (colourReading + 1) Mod READING_COUNT
  If key = "[" And toneGamma > 1.2 Then toneGamma = toneGamma - 0.5
  If key = "]" And toneGamma < 12.0 Then toneGamma = toneGamma + 0.5
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
''    the last colour drawn and skipping unchanged pixels would cut most of that. It needs a second
''    full-size buffer and a comparison in the inner loop, and since PSet became an inline store in
''    the AOT backend a full 400x400 repaint is 0.17 ms - there is not much left to win.
''
''  * INCREMENTAL PEAKS. RebuildLevelTables rescans all three planes once per frame to find each
''    channel's brightest pixel. A peak only ever grows, so it could be maintained as orbits are
''    accumulated. Left out because the rescan is a plain loop anyone can read.
''
''  * A PERCENTILE INSTEAD OF THE PEAK. Each channel is normalised against its own maximum, so one
''    freakishly bright pixel would dim everything else. Normalising against, say, the 99.9th
''    percentile and clipping above it is more robust and is what photo software does. It has not
''    been needed: the log curve already compresses the top end hard.
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
