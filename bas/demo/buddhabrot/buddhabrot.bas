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
''  Keys while it runs:  H help · SPACE pause · R restart · C reading · , . gamma · - + iterations
''                       Z X zoom · W A S D pan · 0 home · P save a still · Q quit
''                       click or wheel on the picture: zoom in on the point you are looking at
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

''  The view is a SQUARE centred somewhere on the plane, so it needs three numbers rather than four.
''  The whole figure sits in a square 2.7 across centred at (-0.65, 0).
Const HOME_CENTRE_REAL      = -0.65
Const HOME_CENTRE_IMAGINARY =  0.0
Const HOME_HALF_SPAN        =  1.35

Dim Shared As Double viewCentreReal, viewCentreImaginary, viewHalfSpan
Dim Shared As Double viewRealMin, viewImaginaryMin

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
#if __SB_WASM__
  '' ⛔ IN THE BROWSER THE OVERLAY IS THE PAGE'S, NOT THE FRAMEBUFFER'S. `Draw String` is not covered
  '' by the WASM backend, and the backend refuses an opcode for being PRESENT in the program rather
  '' than for being reached - so the band, and everything drawn in it, is absent from the source the
  '' module is compiled from. The picture is the same picture; only the strip above it is gone.
  Const TEXT_BAND_HEIGHT = 0
#else
''  ⛔ THE OVERLAY HAS A WIDTH BUDGET, AND IT WAS BEING SPENT WITHOUT COUNTING. `Draw String` uses the
''  built-in 8x8 font, so a 400-pixel window holds exactly FIFTY characters and everything past the
''  fiftieth is simply not drawn - silently. Two lines were carrying seven fields between them and
''  the ends were being cut off. Three shorter lines now, and each field is chosen to fit the
''  narrowest window the demo is run at rather than the widest.
Const TEXT_BAND_HEIGHT  = 30         '' three 8-pixel lines at y = 2, 11, 20, with room to breathe
#endif
''  The frame rate is a BUDGET, not a limit. Every frame is given the same slice of wall-clock time
''  whatever engine is underneath, and the sampling stops when the slice is spent - so this number
''  is what the demo runs at, not what it can manage. fps= moves it.
''
''  📊 Measured 2 Sep 2026 at the default 400x400, holding 60: the interpreter and the JIT both hold
''  58 frames a second, the AOT holds 44. The budget covers BOTH halves of the frame, so an engine
''  that cannot hold the rate is one whose PAINTING is over budget - and under --aot the pixel loop
''  costs 20.2 ms a frame against the interpreter's 7.3. ⛔ That is the engine losing where it should
''  win, not a property of the demo: it is a call to ColourAt per pixel, and a call the AOT does not
''  inline is a helper round-trip that flushes and reloads every allocated register. Logged as open
''  AOT work; the same family as C5-C9 in job/markdown/AOT-PRESTAZIONI.md.
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

''  ⛔⛔ THE BANDS ARE FRACTIONS OF THE CEILING, SO THEY HAVE TO MOVE WITH IT. They were computed
''  once at startup and the +/- keys changed `maximumIterations` without them, which is a defect you
''  can watch happen: drop the ceiling from 5 000 to 624 and RED EMPTIES, because red is still
''  "lived longer than 500 steps" while nothing can now live longer than 624. Reported as "lower the
''  iterations and the red stops appearing, and the composition is softer" - and softer it is, but
''  it was showing two thirds of a picture. One Sub, called from every place that sets the ceiling.
Sub SetIterationCeiling( ByVal n As Integer )
  If n < 50 Then n = 50
  If n > ITERATION_CEILING Then n = ITERATION_CEILING
  maximumIterations = n
  greenCeiling = maximumIterations \ 10
  blueCeiling  = maximumIterations \ 100
  If greenCeiling < 2 Then greenCeiling = 2
  If blueCeiling  < 1 Then blueCeiling  = 1
End Sub
Dim Shared As Double  orbitReal(ITERATION_CEILING)
Dim Shared As Double  orbitImaginary(ITERATION_CEILING)
Dim Shared As Double  pixelsPerImaginaryUnit, pixelsPerRealUnit
Dim Shared As LongInt orbitsTraced, orbitsAccumulated


'' ================================================================================================
''  5. ACCUMULATION
'' ================================================================================================
''  One orbit point becomes one increment. Points that fall outside the window are dropped: the
''  orbit is free to wander anywhere in the disc of radius 2, and the window is smaller than that.

''  ⛔⛔ MOVING THE VIEW THROWS THE PICTURE AWAY, and it has to. Every counter in the histogram is a
''  count of visits to a PIXEL, and a pixel means a different piece of the plane the moment the view
''  changes. Keeping the counts and moving the frame would smear the old picture into the new one.
''
''  ⚠️ AND ZOOMING DOES NOT MAKE THE PICTURE SMALLER TO COMPUTE - it makes it far larger. This is the
''  one place where the Buddhabrot behaves unlike every other fractal zoom, and it is worth watching
''  happen. A Mandelbrot zoom narrows both what you draw AND what you compute: fewer pixels, same
''  work each. Here the view narrows but the SAMPLING cannot, because an orbit that crosses your
''  zoomed window may have started anywhere - so you go on tracing the whole plane and throw away
''  everything that misses. Halve the span and about three quarters of the remaining hits go away
''  with it.
''  ⇒ That collapse is exactly what Metropolis-Hastings sampling exists to fix, and why this demo
''    declares itself uniform: watch the peak stop growing at four or five zoom steps in and you have
''    seen the reason for the technique, which is more convincing than being told.

''  ⛔ ZOOMING OUT HAS TO END WHERE ZOOMING IN STARTED. Doubling the span and leaving the centre
''  alone is what a map does, and it is wrong here: back at the full span you are looking at the
''  whole figure through a window centred on wherever you last went, so the figure sits off to one
''  side and only "0" puts it right. Each step out halves the distance to the home centre, which
''  retraces the walk in; and reaching the full span snaps exactly, because "almost centred" is
''  precisely what the eye notices at x1.
''  ⭐ It lives in ONE Sub because three callers zoom out - the X key, the right mouse button and
''  the browser page - and a rule that is written three times is a rule that will be three rules.
Sub ZoomOut()
  viewHalfSpan = viewHalfSpan * 2.0
  viewCentreReal      = HOME_CENTRE_REAL      + (viewCentreReal      - HOME_CENTRE_REAL)      * 0.5
  viewCentreImaginary = HOME_CENTRE_IMAGINARY + (viewCentreImaginary - HOME_CENTRE_IMAGINARY) * 0.5
  If viewHalfSpan >= HOME_HALF_SPAN Then
    viewHalfSpan        = HOME_HALF_SPAN
    viewCentreReal      = HOME_CENTRE_REAL
    viewCentreImaginary = HOME_CENTRE_IMAGINARY
  End If
End Sub

Sub RecomputeView()
  viewRealMin      = viewCentreReal      - viewHalfSpan
  viewImaginaryMin = viewCentreImaginary - viewHalfSpan
  pixelsPerImaginaryUnit = imageSize / (2.0 * viewHalfSpan)
  pixelsPerRealUnit      = imageSize / (2.0 * viewHalfSpan)
End Sub

Sub AccumulateOrbitPoint( ByVal zReal As Double, ByVal zImaginary As Double, _
                          ByVal planeBase As Integer )
  Dim As Integer column = Int( (zImaginary - viewImaginaryMin) * pixelsPerImaginaryUnit )
  Dim As Integer row    = Int( (zReal      - viewRealMin)      * pixelsPerRealUnit )
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

''  ⛔⛔ AND IT IS ONE FLAT TABLE WITH A STRIDE, NOT A TWO-DIMENSIONAL ONE - which is the same shape
''  `histogram` above already uses, and here it is worth 30x. Written as `levelOfCount(channel, count)`
''  over a REDIM'd matrix, this demo ran at 2 frames a second where it now runs at 58: an element of an
''  array whose dimension SIZES are only known at run time is reached through a push-push-resolve
''  sequence that leaves the C hot loop on every access, and a repaint does three of them per pixel.
''  📊 Measured 2 Sep 2026, one million reads: 5 ns from a fixed-size matrix, 236 ns from a REDIM'd one.
''  ⇒ This is a DEFECT of the engine and it is written down as one (job/markdown/AOT-PRESTAZIONI.md,
''  and the probe that isolates it is job/tests/bench/multidim_index.bas). The demo is flattened here
''  rather than left slow because a showcase that stutters teaches the wrong thing - but the flattening
''  is declared, not quiet, and it comes out when the engine is fixed.
Dim Shared As Integer levelOfCount()   '' channel * levelStride + count -> 0..255
Dim Shared As Integer levelStride = 0  '' entries per channel

''  ⛔⛔ WHAT THE PICTURE IS NORMALISED AGAINST, AND WHY IT IS NO LONGER THE MAXIMUM.
''  Each channel used to be divided by its own brightest pixel. That single pixel is the noisiest
''  statistic in the whole picture: it JUMPS - measured over twenty frames at 200x200 the red
''  maximum went 166, 280, 412, 803, 946 - and every jump renormalises EVERY pixel downwards at
''  once. What that looks like is the thing to understand, because it is not subtle: on frame 12 of
''  that run 23 240 pixels of 40 000 got DARKER while 13 372 got brighter, and in the first frames
''  thousands dropped by more than twelve levels in one step. Early on it is worse still - a channel
''  with almost no data has a maximum of 2 or 3, so everything in it is at full brightness and then
''  collapses as the real range appears. Reported from the demo as "the red parts show up and then
''  suddenly vanish", and "the evolution goes backwards and forwards", which is exactly what it is.
''
''  ⇒ The reference is now a quantity that GROWS SMOOTHLY: counts accumulate in proportion to the
''  orbits drawn, so once a channel has enough evidence its counts-per-orbit is measured ONCE and
''  the reference is that rate times the orbits so far. Nothing is ever renormalised downwards, and
''  a freak pixel above the reference simply clips - which is what photo software does and what this
''  file's own list of "improvements deliberately left out" proposed and dismissed as not needed.
''  ⭐ `norm=peak` restores the old behaviour exactly, which is how the two are compared on one run.
''  The exposure the picture starts from, before there is enough of it to measure one.
Const NORM_FLOOR = 64.0
''  ⭐ AND IT IS A FUNCTION OF THE DATA ALONE - the plane's MEAN, times a constant per channel.
''  That last part is the whole difference between this and the first attempt, which measured a rate
''  once and kept it: WHEN it measured depended on how often the picture was repainted, so still mode
''  and the browser chose different exposures from the same orbits and their pictures parted company
''  (17% of the bytes, up to 149 levels - caught by the net that compares the two). A statistic of
''  the histogram has no such dependence: the same counts give the same reference, always.
''
''  📊 Why the MEAN and not the maximum or a percentile, measured over 120 frames on the red plane:
''      worst one-frame jump   max x1.75   ·   99.9th percentile x1.65   ·   MEAN x1.46
''      frames where it FELL   max 0       ·   99.9th percentile 3       ·   MEAN 0
''  The percentile is not even monotone. The mean is a sum over 40 000 pixels, so it is the quietest
''  statistic available, and it is proportional to the orbits drawn to within 3% across a
''  twenty-five-fold range - which is what makes a constant multiple of it a stable exposure.
''
''  ⚠️ The maximum grows SLOWER than the mean (max/mean falls from 102 to 36 between 200 000 and
''  5 000 000 orbits on the red plane), so this reference drifts against it - and that is fine, and
''  worth knowing why: the tone curve takes a logarithm and then a 1/4.5 power, so a reference 2.8
''  times too high costs under 4% of brightness. The compression that makes the picture readable is
''  the same compression that makes the exposure forgiving.
''  ⭐ CALIBRATED, not chosen: these are max/mean measured on a two-million-orbit still, per channel,
''  which is the exposure the published picture was made at. They differ by a factor of seven between
''  the channels because the three planes have very different shapes - the blue one is a haze whose
''  brightest pixel is only five times its mean, the red one is structure standing out of almost
''  nothing.
Const NORM_K_RED   = 40.4     '' reference = K * the plane's mean
Const NORM_K_GREEN = 19.5
Const NORM_K_BLUE  =  5.5
Dim Shared As Integer stableNormalise  '' 1 = the reference above, 0 = the old per-channel maximum

Function NormaliseAgainst( ByVal channel As Integer, ByVal planeTotal As LongInt, _
                           ByVal planePeak As LongInt ) As Double
  If stableNormalise = 0 Then Return planePeak
  Dim As Double k = NORM_K_RED
  If channel = 1 Then k = NORM_K_GREEN
  If channel = 2 Then k = NORM_K_BLUE
  Dim As Double reference = k * planeTotal / pixelsPerPlane
  '' ⛔ AND A FLOOR, because early on the mean is a fraction of one count and every pixel would clip
  '' to white and then resolve downwards - which is the same "shows up and then vanishes" the
  '' maximum produced, arrived at from the other side. Below the floor the picture starts DARK and
  '' only ever brightens, which is what the evidence itself does.
  If reference < NORM_FLOOR Then reference = NORM_FLOOR
  Return reference
End Function

Sub RebuildLevelTables()
  Dim As Integer channel, biggest = 1
  Dim As LongInt peak(2), planeTotal(2)
  Dim As Integer i
  For channel = 0 To 2
    peak(channel) = 1
    planeTotal(channel) = 0
    For i = 0 To pixelsPerPlane - 1
      Dim As LongInt cnt = histogram(channel * pixelsPerPlane + i)
      planeTotal(channel) = planeTotal(channel) + cnt
      If cnt > peak(channel) Then peak(channel) = cnt
    Next i
    If peak(channel) > biggest Then biggest = peak(channel)
  Next channel

  If levelStride < biggest + 1 Then
    levelStride = biggest + biggest \ 2 + 1
    ReDim levelOfCount(3 * levelStride - 1)
  End If
  For channel = 0 To 2
    Dim As Double reference = NormaliseAgainst(channel, planeTotal(channel), peak(channel))
    Dim As Double inverseLogOfPeak = 1.0 / Log(1.0 + reference)
    Dim As Integer channelBase = channel * levelStride
    levelOfCount(channelBase) = 0
    Dim As LongInt count
    For count = 1 To peak(channel)
      levelOfCount(channelBase + count) = Int( 255.0 * ClampUnit( BrightnessOf(count, inverseLogOfPeak) ) )
    Next count
    '' Anything above this channel's own peak cannot occur in it, but the table is shared in size.
    For count = peak(channel) + 1 To levelStride - 1
      levelOfCount(channelBase + count) = 255
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
  Dim As Integer longLived  = levelOfCount(histogram(pixel))
  Dim As Integer midLived   = levelOfCount(levelStride + histogram(pixelsPerPlane + pixel))
  Dim As Integer shortLived = levelOfCount(2 * levelStride + histogram(2 * pixelsPerPlane + pixel))

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
#if __SB_WASM__
  '' ⛔ A module has no filesystem, and the backend says so itself rather than emitting something
  '' that runs and lies: "WebAssembly has no filesystem: a module cannot open, inspect or change
  '' files". So the body is absent here, not merely unreached. The browser build never calls it.
#else
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
#endif
End Sub

''  ⛔⛔ EVERYTHING THAT BELONGS TO ONE FRAME HAPPENS INSIDE ONE LOCK, the overlay included.
''  While a lock is held nothing is presented; the unlock presents once. Drawing the text after the
''  unlock - which is where it was first written - presents the frame THREE times, once for the
''  image and once for each line of text, and the picture and the numbers written over it are never
''  shown as the same instant.
''  The band is cleared before it is written, because a number that gets shorter would otherwise
''  leave the tail of the longer one it replaced standing behind it.

''  ⭐ ONE painting routine, not two. The browser build differs from the native one by what is
''  COMPILED OUT of it - the lock and the two lines of text - and by nothing else, so the loop that
''  decides every pixel cannot drift between them. That is the same discipline the voxel demo keeps
''  for its camera path, and for the same reason: two descriptions of one thing become two things.
''  ⭐ THE OVERLAY IS BUILT TO FIT, FIELD BY FIELD, instead of being written out and then cut off.
''  `Draw String` uses the built-in 8x8 font, so the window holds imageSize\8 characters - fifty at
''  the default 400, twenty-five at size=200 - and anything past that is silently not drawn. Adding a
''  field only when it still fits means a narrow window loses whole fields, in order of importance,
''  rather than losing the end of whatever happened to be last.
Function Fits( ByVal line As String, ByVal extra As String ) As String
  If Len(line) + Len(extra) <= imageSize \ 8 Then Return line + extra
  Return line
End Function

#if __SB_WASM__
  '' The browser build's help is the page around it - `Draw String` is not covered by the WASM
  '' backend, and the backend refuses an opcode for being PRESENT rather than for being reached.
#else
Dim Shared As Integer helpVisible

''  ⭐ THE KEYS ARE CHOSEN FOR THE KEYBOARD, NOT FOR THE ALPHABET. The tone curve used to be on
''  `[` and `]`, which on an Italian layout need AltGr - a chord for something you nudge back and
''  forth while looking at the picture. `,` and `.` are unshifted on both an Italian and a US
''  layout, and so is `-`; `+` is unshifted on Italian and shifted on US, so `=` does the same job
''  on the key it shares there. The old brackets still work, because someone has learnt them.
''  The longest line below, in characters. `Draw String` clips without saying so - the same silent
''  cut that was taking the ends off the overlay - so a window too narrow to hold the list gets a
''  sentence that fits instead of a list that does not.
Const HELP_WIDEST = 38

Sub DrawHelp()
  Dim As Integer w = imageSize - 24
  Dim As Integer h = 158
  Dim As Integer x0 = 12
  Dim As Integer y0 = TEXT_BAND_HEIGHT + 14
  If w < 8 Then Exit Sub
  If (w - 16) \ 8 < HELP_WIDEST Then
    Line (x0, y0)-(x0 + w - 1, y0 + 20), RGB(0, 0, 0), BF
    Line (x0, y0)-(x0 + w - 1, y0 + 20), RGB(90, 90, 100), B
    Draw String (x0 + 6, y0 + 7), "keys: run with help=1", RGB(224, 129, 63)
    Exit Sub
  End If
  Line (x0, y0)-(x0 + w - 1, y0 + h - 1), RGB(0, 0, 0), BF
  Line (x0, y0)-(x0 + w - 1, y0 + h - 1), RGB(90, 90, 100), B
  Dim As Integer ln = y0 + 8
  Draw String (x0 + 8, ln), "KEYS", RGB(224, 129, 63) : ln += 14
  Draw String (x0 + 8, ln), "H         this help - any key closes it", RGB(220, 220, 220) : ln += 10
  Draw String (x0 + 8, ln), "SPACE     pause / resume", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "R         restart, same seed", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "P         save a still", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "Q         quit", RGB(200, 200, 200) : ln += 14
  Draw String (x0 + 8, ln), "C         reading: NEBULA AURORA EMBER", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), ",  .      tone curve down / up", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "-  +      iterations halve / double", RGB(200, 200, 200) : ln += 14
  Draw String (x0 + 8, ln), "Z  X      zoom in / out", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "W A S D   pan     0  whole figure", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "mouse     click zooms in, right out", RGB(200, 200, 200) : ln += 10
  Draw String (x0 + 8, ln), "          the wheel does both", RGB(150, 150, 150)
End Sub
#endif

Sub PaintFrame( ByVal topLine As String, ByVal middleLine As String, ByVal bottomLine As String )
  RebuildLevelTables()
  Dim As Integer x, y
#if __SB_WASM__
  '' No lock: there is no presenter to hold back. The page reads the framebuffer out of linear
  '' memory when it is ready, which is after this returns.
#else
  ScreenLock
#endif
  For y = 0 To imageSize - 1
    For x = 0 To imageSize - 1
      PSet (x, y + TEXT_BAND_HEIGHT), ColourAt( y * imageSize + x )
    Next x
  Next y
#if __SB_WASM__
  '' The overlay is HTML in the browser build - see TEXT_BAND_HEIGHT.
#else
  Line (0, 0)-(imageSize - 1, TEXT_BAND_HEIGHT - 1), RGB(0, 0, 0), BF
  Draw String (4, 2),  topLine,    RGB(255, 255, 255)
  Draw String (4, 11), middleLine, RGB(170, 170, 170)
  Draw String (4, 20), bottomLine, RGB(140, 140, 140)
  '' Inside the lock, like the rest of the overlay: drawing it after the unlock would present the
  '' frame twice and show the picture and the panel as two different instants.
  If helpVisible <> 0 Then DrawHelp()
  ScreenUnlock
#endif
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
  Print "  series=N    also write N numbered stills on the way, spaced geometrically"
  Print "  gamma=N     tone curve applied after the log        (default 4.5)"
  Print "  norm=X      stable | peak - what each channel is divided by (default stable)"
  Print "  palette=X   nebula | aurora | ember                  (default nebula)"
  Print "  fps=N       frames per second to hold                 (default 60)"
  Print "  re=N im=N   centre of the view                       (default -0.65, 0)"
  Print "  zoom=N      how many times to magnify                 (default 1)"
  Print
  Print "Keys while running (H shows them on the picture too):"
  Print "  SPACE pause    R restart    P save a still    Q quit"
  Print "  C reading      , . gamma    - + iterations"
  Print "  Z X zoom       W A S D pan  0 back to the whole figure"
  Print "  left click or wheel up: zoom in on the pointer   right click or wheel down: zoom out"
End Sub


#if __SB_WASM__
'' ================================================================================================
''  9b. THE BROWSER BUILD - the same program, driven by the page instead of by a keyboard
'' ================================================================================================
''  ⛔ THE MODULE MUST NOT OWN THE LOOP. A module that ran until Q was pressed would freeze the tab:
''  there is no Q, and nothing else gets to run while a wasm call is on the stack. So `main` sets up
''  and draws ONE frame, and the page calls PROC_STEPFRAME once per animation tick - which is also
''  what makes the browser's own frame clock, rather than a Timer inside BASIC, the thing that paces
''  it. The voxel-landscape demo is driven the same way, for the same reason.
''
''  ⭐ AND THE PAGE PASSES THE ORBIT COUNT rather than a time budget. Natively the budget is a slice
''  of wall clock because the point there is to hold the frame rate CONSTANT across three engines;
''  here there is one engine, and a count the page chose is a count the page can divide by its own
''  measured milliseconds. The honest number in a browser is orbits per second, and this is how it
''  is computed without the module timing itself.
Dim Shared As LongInt browserSeed

Sub StepFrame( ByVal orbitsThisFrame As Integer )
  Dim As Integer i
  For i = 1 To orbitsThisFrame
    TraceOneOrbit()
  Next i
  PaintFrame("", "", "")
End Sub

''  Everything the keyboard and the mouse do natively, as one entry point. It is ONE Sub and not six
''  because a Sub small enough to be inlined is a Sub that does not survive as an exported function -
''  and the page can only call what the module exports.
''    0 reading (a = 0..2)   1 gamma in tenths   2 zoom in at pixel (a, b)   3 zoom out
''    4 home                 5 restart           6 iteration ceiling = a
Sub Control( ByVal command As Integer, ByVal a As Integer, ByVal b As Integer )
  Dim As Integer moved = 0
  If command = 0 Then
    colourReading = a Mod READING_COUNT
  ElseIf command = 1 Then
    toneGamma = a / 10.0
    If toneGamma < 1.2  Then toneGamma = 1.2
    If toneGamma > 12.0 Then toneGamma = 12.0
  ElseIf command = 2 Then
    '' The inverse of the mapping AccumulateOrbitPoint uses, exactly as the native mouse zoom does:
    '' a pixel of the canvas IS a point of the complex plane.
    If a >= 0 And a < imageSize And b >= 0 And b < imageSize Then
      viewCentreImaginary = viewImaginaryMin + a / pixelsPerImaginaryUnit
      viewCentreReal      = viewRealMin + b / pixelsPerRealUnit
      viewHalfSpan = viewHalfSpan / 2.0
      moved = 1
    End If
  ElseIf command = 3 Then
    ZoomOut() : moved = 1
  ElseIf command = 4 Then
    viewCentreReal = HOME_CENTRE_REAL : viewCentreImaginary = HOME_CENTRE_IMAGINARY
    viewHalfSpan = HOME_HALF_SPAN : moved = 1
  ElseIf command = 5 Then
    moved = 1
  ElseIf command = 6 Then
    SetIterationCeiling(a)
    moved = 1
  End If
  If viewHalfSpan > HOME_HALF_SPAN Then viewHalfSpan = HOME_HALF_SPAN
  '' Every move throws the counts away and starts the seed again, so what appears after a move is a
  '' fresh picture of the new window rather than the old one dragged into it. Same rule as natively.
  If moved <> 0 Then
    RecomputeView()
    Dim As Integer i
    For i = 0 To 3 * pixelsPerPlane - 1 : histogram(i) = 0 : Next i
    SeedRandom(browserSeed) : orbitsTraced = 0 : orbitsAccumulated = 0
  End If
  '' ⛔ AND THE MODULE SAYS WHERE THE VIEW IS, rather than letting the page work it out. The page has
  '' no other way to know - a Sub returns nothing - and the alternative is for the page to keep its
  '' own copy of the mapping from a pixel to the plane, which is a SECOND description of the one
  '' thing this file is careful to describe once. Printing it costs a line per command, and it is
  '' also what tells the reader that a tap landed at all when the fresh window is still nearly empty.
  Print "view "; HOME_HALF_SPAN / viewHalfSpan; " "; viewCentreReal; " "; viewCentreImaginary
End Sub
#endif


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
'' `norm=peak` is the old behaviour - each channel divided by its own brightest pixel - kept so the
'' two can be compared on one run. The default is the reference described beside NormaliseAgainst.
stableNormalise = IIf( LCase(ArgumentValue("norm", "stable")) = "peak", 0, 1 )
colourReading = READING_NEBULA
If LCase(ArgumentValue("palette", "")) = "aurora" Then colourReading = READING_AURORA
If LCase(ArgumentValue("palette", "")) = "ember"  Then colourReading = READING_EMBER
Dim As Double targetFrameSeconds = 1.0 / CDbl( ArgumentValue("fps", Str(DEFAULT_FRAMES_PER_SECOND)) )

If maximumIterations > ITERATION_CEILING Then maximumIterations = ITERATION_CEILING

'' The two inner ceilings, a decade apart, which is the proportion the classic three-pass version
'' uses (5000 / 500 / 50). Deriving them from max= keeps one knob instead of three: raising the
'' ceiling deepens all three channels together, which is what anyone turning that dial means.
SetIterationCeiling(maximumIterations)

pixelsPerPlane = imageSize * imageSize
ReDim histogram(3 * pixelsPerPlane - 1)
levelStride = 4096
ReDim levelOfCount(3 * levelStride - 1)
viewCentreReal      = CDbl( ArgumentValue("re",   Str(HOME_CENTRE_REAL)) )
viewCentreImaginary = CDbl( ArgumentValue("im",   Str(HOME_CENTRE_IMAGINARY)) )
viewHalfSpan        = HOME_HALF_SPAN / CDbl( ArgumentValue("zoom", "1") )
RecomputeView()
SeedRandom(seed)
orbitsTraced = 0
orbitsAccumulated = 0

#if __SB_WASM__
'' ---- the browser build: set up, draw one frame, hand back ---------------------------------------
''  There is no still mode here (no file to write) and no live loop (the page owns the clock).
browserSeed = seed
ScreenRes imageSize, imageSize + TEXT_BAND_HEIGHT, 32
''  ⛔ AND THIS CALL IS NOT DECORATION. A Sub that nothing calls does not survive as a function -
''  it is eliminated before the backend sees it - so the page would have nothing to call. Command -1
''  matches no branch and does nothing; what it does is give Control a call site. Verified by
''  looking at the module's export list, which is where its absence showed.
Control(-1, 0, 0)
StepFrame(20000)          '' one frame, so the canvas is never blank before the first tick
End
#endif

'' ---- still mode: no window, fixed work, one file. This is what the determinism check runs. -----
If stillOrbits > 0 Then
  Dim As Double startedAt = Timer

  '' series=N writes N numbered stills on the way to the finish instead of one at the end, which is
  '' how the convergence recording is made: one run, every frame from the same orbits, so the film
  '' is the SAME computation the final picture came from rather than N runs that happen to agree.
  ''
  '' ⚠️ THE SPACING IS THE DIFFERENCE BETWEEN A FILM AND A SLIDESHOW, and evenly spaced frames are
  '' the wrong answer twice over. Convergence is fast at the start and slow at the end: a picture at
  '' two million orbits differs from one at one million far more than twenty million differs from
  '' nineteen, so even spacing spends nearly its whole length on the part where nothing changes.
  ''
  '' Frame k of N is taken at  still * (k/N)^p , with p chosen so the first frame lands at a
  '' FIVE-THOUSANDTH of the finish. ⚠️ A five-hundredth was the first try and it starts the film with
  '' the figure already recognisable - which throws away the best part, the emergence out of noise. Two things follow, and both are what the eye wants:
  ''   - early frames are close together in ORBITS and far apart in appearance;
  ''   - the relative step is p/k, so it shrinks as the film runs and the picture SETTLES rather
  ''     than being cut off mid-climb. A purely geometric ratio - which is what this did first -
  ''     keeps the step constant to the last frame, and the film ends still visibly moving.
  Dim As Integer frameCount = CInt( ArgumentValue("series", "0") )
  Dim As Integer frameIndex = 0
  Dim As Double  framePower = 1.0, nextFrameAt = 0.0
  If frameCount > 0 Then
    framePower  = Log(5000.0) / Log(CDbl(frameCount))
    nextFrameAt = stillOrbits * ((1.0 / CDbl(frameCount)) ^ framePower)
  End If

  Do While orbitsTraced < stillOrbits
    TraceOneOrbit()
    If frameCount > 0 And orbitsTraced >= nextFrameAt Then
      frameIndex = frameIndex + 1
      WritePortablePixmap( outputName + "_" + Right("000" + Str(frameIndex), 3) + ".ppm" )
      If frameIndex >= frameCount Then
        nextFrameAt = stillOrbits * 2.0                                  '' no more
      Else
        nextFrameAt = stillOrbits * ((CDbl(frameIndex + 1) / CDbl(frameCount)) ^ framePower)
      End If
    End If
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

'' ---- the pointer -------------------------------------------------------------------------------
'' GETMOUSE answers a STATUS (0 = there is a mouse over our window, 1 = there is not) and fills the
'' fields by reference. Everything below is guarded on that status, so the demo behaves exactly as
'' it always did when it is run without a pointer over it - which is also how the determinism guard
'' runs it.
''
'' ⚠️ THE BUTTONS ARE A LEVEL, NOT AN EVENT. Read once a frame at sixty frames a second, a button
'' held down for a fifth of a second is twelve readings: acting on the level would zoom twelve times
'' and land on a window a four-thousandth of the one you clicked in. So the previous reading is kept
'' and only the RISING EDGE counts - press once, zoom once. The wheel is the opposite kind of thing:
'' it is a running COUNTER of notches, so what matters is the difference since the last frame.
Dim As Integer pointerX, pointerY, pointerWheel, pointerButtons, pointerStatus
Dim As Integer previousButtons = 0, previousWheel = 0
Dim As Integer pointerOverPicture = 0
Dim As Double  pointerReal = 0.0, pointerImaginary = 0.0
pointerStatus = GetMouse(pointerX, pointerY, pointerWheel, pointerButtons)
If pointerStatus = 0 Then previousWheel = pointerWheel

Do
  Dim As Double frameStart = Timer
  framesDrawn = framesDrawn + 1

  If paused = 0 And helpVisible = 0 Then
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

  '' Where is the pointer, and what does it point AT? The picture is a window on the complex plane,
  '' so a pixel has a coordinate: this is the inverse of the mapping AccumulateOrbitPoint uses to put
  '' an orbit point into the histogram, with the text band's height taken off the row.
  '' ⚠️ The window is sized to the framebuffer by the presenter, so a mouse pixel IS a picture pixel -
  '' there is no scale factor to undo here, and if the window is ever made resizable there will be.
  pointerStatus = GetMouse(pointerX, pointerY, pointerWheel, pointerButtons)
  pointerOverPicture = 0
  If pointerStatus = 0 And pointerY >= TEXT_BAND_HEIGHT And pointerX >= 0 _
     And pointerX < imageSize And pointerY - TEXT_BAND_HEIGHT < imageSize Then
    pointerOverPicture = 1
    pointerImaginary = viewImaginaryMin + pointerX / pixelsPerImaginaryUnit
    pointerReal      = viewRealMin + (pointerY - TEXT_BAND_HEIGHT) / pixelsPerRealUnit
  End If

  '' Draw String writes into the framebuffer and nowhere else. LOCATE + PRINT would also work, but
  '' PRINT echoes to standard output as well, and a demo that scrolls a thousand lines up the
  '' terminal it was launched from is not one anybody runs twice.
  Dim As Double paintStart = Timer
  '' Three lines, each inside the fifty characters a 400-pixel window holds. The pointer readout
  '' REPLACES the view on the last line while the pointer is over the picture, rather than being
  '' appended to it: two coordinate pairs on one line is what pushed the old overlay off the edge.
  '' Most important first: whichever fields a narrow window cannot hold are the ones dropped.
  Dim As String line1 = label + IIf(paused, " [PAUSED]", "") + IIf(helpVisible, " [HELP]", "")
  line1 = Fits(line1, " " + Str(Int(orbitsPerSecond)) + "/s")
  line1 = Fits(line1, "  " + ReadingName(colourReading) + " g" + Format(toneGamma, "0.0"))
  line1 = Fits(line1, " iter " + Str(maximumIterations))

  Dim As String line2 = "traced " + Str(orbitsTraced)
  line2 = Fits(line2, "  drawn " + Str(orbitsAccumulated))
  line2 = Fits(line2, "  peak " + Str(PeakRedCount()))

  '' The pointer readout REPLACES the view while the pointer is over the picture: two coordinate
  '' pairs on one line is what pushed the old overlay off the edge in the first place.
  Dim As String line3
  If pointerOverPicture <> 0 Then
    line3 = "pointer " + Format(pointerReal, "0.0000") + " " + Format(pointerImaginary, "0.0000")
  Else
    line3 = "x" + Format(HOME_HALF_SPAN / viewHalfSpan, "0.#")
    line3 = Fits(line3, " @ " + Format(viewCentreReal, "0.0000") + " " + Format(viewCentreImaginary, "0.0000"))
  End If

  PaintFrame(line1, line2, line3)
  paintSeconds = Timer - paintStart

  key = LCase(InKey)

  '' ⛔ WHILE THE HELP IS UP, EVERY KEY MEANS "CLOSE IT" AND NOTHING ELSE. Letting the keys through
  '' would have you changing the gamma of a picture you cannot see, and finding it changed when the
  '' panel goes away.
  If helpVisible <> 0 Then
    If key <> "" Then helpVisible = 0
    key = ""
  ElseIf key = "h" Then
    helpVisible = 1
    key = ""
  End If

  If key = "q" Or key = Chr(27) Then Exit Do
  If key = " " Then paused = 1 - paused
  '' Every move throws the counts away and starts the seed again, so what you see after a move is a
  '' fresh picture of the new window rather than the old one dragged into it.
  Dim As Integer moved = 0
  If key = "r" Then moved = 1
  If key = "z" Then viewHalfSpan = viewHalfSpan / 2.0 : moved = 1
  If key = "x" Then ZoomOut() : moved = 1
  '' The picture is turned a quarter turn, so the keys are too: W and S walk the REAL axis, which
  '' runs down the screen, and A and D walk the IMAGINARY one, which runs across it.
  If key = "w" Then viewCentreReal      = viewCentreReal      - viewHalfSpan / 2.0 : moved = 1
  If key = "s" Then viewCentreReal      = viewCentreReal      + viewHalfSpan / 2.0 : moved = 1
  If key = "a" Then viewCentreImaginary = viewCentreImaginary - viewHalfSpan / 2.0 : moved = 1
  If key = "d" Then viewCentreImaginary = viewCentreImaginary + viewHalfSpan / 2.0 : moved = 1
  If key = "0" Then
    viewCentreReal = HOME_CENTRE_REAL : viewCentreImaginary = HOME_CENTRE_IMAGINARY
    viewHalfSpan = HOME_HALF_SPAN : moved = 1
  End If

  '' ---- the same three moves, from the pointer ----------------------------------------------
  '' W A S D walk the view by half a window at a time, which is the only thing a keyboard can do:
  '' to reach a filament you can SEE you have to walk towards it and correct, and each correction
  '' throws the picture away and starts it again. Pointing at it is one move instead of six.
  ''
  '' Zooming IN takes the pointer as the new centre - that is the whole point of it. Zooming OUT
  '' leaves the centre alone: the interesting thing is already in the middle, and pulling it towards
  '' the pointer as you widen would walk the view off it.
  Dim As Integer pressed = pointerButtons And (Not previousButtons)   '' rising edges only
  Dim As Integer wheelStep = pointerWheel - previousWheel
  If pointerStatus = 0 Then
    previousButtons = pointerButtons
    previousWheel = pointerWheel
  End If
  If pointerOverPicture <> 0 Then
    If (pressed And 1) <> 0 Or wheelStep > 0 Then
      viewCentreReal = pointerReal : viewCentreImaginary = pointerImaginary
      viewHalfSpan = viewHalfSpan / 2.0 : moved = 1
    End If
    If (pressed And 2) <> 0 Or wheelStep < 0 Then
      ZoomOut() : moved = 1
    End If
  End If

  If viewHalfSpan > HOME_HALF_SPAN Then viewHalfSpan = HOME_HALF_SPAN
  If moved <> 0 Then
    RecomputeView()
    Dim As Integer i
    For i = 0 To 3 * pixelsPerPlane - 1 : histogram(i) = 0 : Next i
    SeedRandom(seed) : orbitsTraced = 0 : orbitsAccumulated = 0 : startedAt = Timer
  End If
  '' ⚠️ This used to be S, which is also the key that pans DOWN: one press did both, so every save
  '' wrote a picture of the view you had just left. P for picture.
  If key = "p" Then WritePortablePixmap(outputName)
  '' The tone curve and the reading are the two things worth changing while looking at the picture,
  '' because both are judgements about what you want to SEE and neither costs a recomputation - the
  '' orbits are already counted, only the tables are rebuilt.
  If key = "c" Then colourReading = (colourReading + 1) Mod READING_COUNT
  If (key = "," Or key = "[") And toneGamma > 1.2  Then toneGamma = toneGamma - 0.5
  If (key = "." Or key = "]") And toneGamma < 12.0 Then toneGamma = toneGamma + 0.5
  If key = "+" Or key = "=" Then SetIterationCeiling(maximumIterations * 2)
  If key = "-" Then SetIterationCeiling(maximumIterations \ 2)

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
