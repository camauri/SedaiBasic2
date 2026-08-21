'' ================================================================================================
''  BUBBLE UNIVERSE - a real-time SedaiBasic demo
'' ================================================================================================
''
''  CREDIT, because none of the mathematics here is ours.
''    * The algorithm is yuruyura's, posted on X in February 2020:
''          https://x.com/yuruyurau/status/1226846058728177665
''    * Paul Dunn ported it to BASIC and gave it the name "Bubble Universe".
''    * Mike Markowski (mike.ab3ap@gmail.com) rewrote it in Python in August 2026 using complex
''      phasors with an explicit feedback term, and separated calculation from plotting:
''          https://udel.edu/~mm/bubble/          (spirals.py)
''  This file follows MARKOWSKI's formulation. What is ours is the translation into SedaiBasic, the
''  tabulated palette and the real-time loop. Verified identical to spirals.py, pixel for pixel.
''
''  ------------------------------------------------------------------------------------------
''  WHAT YOU ARE LOOKING AT
''  ------------------------------------------------------------------------------------------
''  Despite the name there are no soap bubbles and no physics: this is a purely mathematical object.
''  M spirals are traced simultaneously. Each spiral walks NPTS points, and every point is the sum of
''  two unit-length phasors (points on the unit circle) whose phases are FED BACK from the point
''  produced immediately before it.
''
''  That feedback is the entire effect. Without it, e^(j*a) + e^(j*b) with a and b advancing linearly
''  draws two circles and nothing else. With it, each point displaces the phases that generate the
''  next one, and the trace winds into a spiral whose curvature varies along its own length. Run M of
''  those from M evenly spaced start phases and the spirals interleave into the nested figure.
''
''  ------------------------------------------------------------------------------------------
''  THE RECURRENCE
''  ------------------------------------------------------------------------------------------
''  Spiral i (of M) is defined by two constants:
''        col = i                      - the first phasor's carrier phase
''        phi = t + 2*pi*i/M           - the second phasor's carrier phase, offset by the spiral's
''                                       share of a full turn, and shifted by the animation phase t
''  Its walk starts at a = col, b = phi, and each step is:
''        Z  = e^(j*a) + e^(j*b)       so  Re Z = cos a + cos b ,  Im Z = sin a + sin b
''        plot Z
''        a := col + K * Re Z          - re-modulate the CARRIERS, not the previous phases
''        b := phi + K * Im Z
''
''  Two consequences worth stating, because both are load-bearing further down:
''
''    1. |Z| <= 2 ALWAYS. Each phasor has length exactly 1, so their sum cannot exceed 2 by the
''       triangle inequality. The whole universe therefore lives inside a disc of radius 2 - which is
''       what lets the plotting below need no clipping test at all (see RAD).
''
''    2. THE FEEDBACK IS A MODULATION, NOT A RANDOM WALK. Both updates start again from the spiral's
''       fixed constants col and phi; the point only PERTURBS them. Had they been written
''       "a := a + K*Re Z" the phases would drift without bound, the figure would lose its M-fold
''       symmetry within a few dozen steps and then diverge. This is the single line most likely to
''       be mistranslated, and it does not fail loudly - it fails by slowly becoming noise.
''
''  ------------------------------------------------------------------------------------------
''  WHY THIS DEMO CAN RUN IN REAL TIME WHEN OUR FLUID DEMO COULD NOT
''  ------------------------------------------------------------------------------------------
''  It is not that this algorithm does less arithmetic - it does four transcendental calls per point,
''  which is not cheap. It is the WORKING SET.
''
''  A spiral carries exactly two numbers from one step to the next: a and b. Nothing else survives an
''  iteration. So the live state of the entire figure is 2*M doubles - four kilobytes at M = 250,
''  which sits in L1 cache and never leaves it. The cost is therefore pure computation, and pure
''  computation is what a machine has in abundance.
''
''  Our lattice-Boltzmann fluid demo (bas/demo/sedai_lbm.bas) is the opposite: it streams 5.5 MB of
''  lattice per frame and, measured, would have needed 49.8 GB/s of memory bandwidth to reach 30 fps
''  against 25.6 GB/s physically available on that machine. Real time was not available to ANY
''  implementation of it, in any language - and no amount of faster code could have changed that.
''  ⇒ Before deciding whether a visual idea can be real-time, look at how much state it moves per
''    frame, not at how many operations it performs.
''
''  ------------------------------------------------------------------------------------------
''  ⛔ RUN IT INTERPRETED - THE COMPILED ENGINES ARE SLOWER HERE
''  ------------------------------------------------------------------------------------------
''  Measured 22 Aug 2026 on this machine, 2 M points of exactly this recurrence:
''
''        native FPC -O3 ....... 224 ms      (the ceiling: everyone calls the same libm)
''        sb (interpreted) ..... 355 ms      1.58x off the ceiling
''        sb --aot ............. 630 ms      2.81x off the ceiling
''        sb --jit ............. 627 ms
''
''  The compiled engines are 1.8x SLOWER than the interpreter, and the reason is specific: sin and
''  cos have no native lowering in the AOT or the JIT yet, so every call leaves compiled code through
''  the generic helper, which flushes and reloads every allocated machine register around it - about
''  51 ns per call, against ~30 ns for the transcendental itself. The interpreter has no registers to
''  flush and simply calls libm.
''
''  Note also how close the ceiling is: 1.58x. This workload is dominated by libm, which every
''  language pays alike, so a BASIC interpreter is not at a structural disadvantage here the way it
''  would be in a tight integer loop.
''
''  ------------------------------------------------------------------------------------------
''  USAGE
''  ------------------------------------------------------------------------------------------
''      sb bas/demo/bubble_universe.bas                 run until a key is pressed
''      sb bas/demo/bubble_universe.bas 300             stop after 300 frames (for timing runs)
''      sb bas/demo/bubble_universe.bas 1 dump.ppm      render ONE frame and write it as a PPM
''      sb bas/demo/bubble_universe.bas 1 out.ppm 0.0   ...at a FIXED phase, for comparison
''
''  The third form exists because a still is the only way to inspect the figure when there is no
''  window - but see the warning at DumpFrame: a still is blind to everything that goes wrong
''  BETWEEN frames, so it is a sanity check, never a sign-off.
'' ================================================================================================

'' ---- the four numbers that define the figure -------------------------------------------------
Const N      = 500          '' output is N x N pixels - the size the author's own page animates at.
                            '' It is a density knob, not just a size: M*NPTS points are spread over
                            '' N^2 pixels, so a smaller N piles many points onto each pixel and the
                            '' figure turns to mush (visible by N = 200), while a larger one thins the
                            '' arms until they break into dots. 400-500 is where the arms stay
                            '' continuous and the disc still fills the frame.
Const M      = 250          '' spirals. Raising M adds arms and costs time linearly.
Const NPTS   = 250          '' points traced along each spiral, per frame. Raising it lengthens the
                            '' arms - the spiral simply keeps winding - and also costs linearly.
Const K      = 1.0          '' FEEDBACK INTENSITY, and the one knob that changes the figure rather
                            '' than its size. Markowski's note, which matches what we saw: values
                            '' near 1 are the most interesting, and small changes alter the result
                            '' completely. K = 1 is the value that generates the original Universe;
                            '' K = 0 collapses it to two circles, since the feedback term vanishes.

Const TWOPI  = 6.283185307179586

'' Animation speed, in phase units per SECOND OF WALL CLOCK. Markowski advances t by 1/30 per frame
'' at a 30 Hz frame rate, i.e. exactly 1.0 per second; we keep that speed but drive t from the clock
'' instead of from the frame counter. The difference matters on a machine that cannot hold the target
'' rate: a per-frame step would make the figure evolve in slow motion instead of dropping frames,
'' which is the wrong failure - the viewer should see the same animation, less smoothly.
Const PHASE_RATE = 1.0

'' ---- plotting geometry -----------------------------------------------------------------------
'' A point is drawn at c + RAD*Z with |Z| <= 2 (see THE RECURRENCE), so the disc exactly fills the
'' square: RAD = N/4 puts the extremes at 0 and N. These are the reference's constants, and keeping
'' them is what makes this translation verifiable rather than merely plausible - rendered at a fixed
'' phase, our frame is IDENTICAL to spirals.py, pixel for pixel and colour for colour (100%/100%,
'' 17 245 lit pixels). Any "improvement" here costs that check.
''
'' ⛔ TWO THINGS WERE WRONG IN THE FIRST VERSION OF THIS DEMO, and both are worth recording because
'' neither announced itself.
''
''   1. Int() IS NOT OPTIONAL. Handing PSet a Double lets the dialect convert it, and MODERN ROUNDS
''      while the reference (numpy's .astype(int)) TRUNCATES. Half a pixel of systematic shift: the
''      lit-pixel COUNT still matched to 0.16%, so the figure looked right, but only 47% of the
''      pixels were in the same place. A statistic that matches is not a picture that matches.
''
''   2. RAD = (N-1)/4 was a "free optimisation" that was neither. The idea was to shrink the figure by
''      half a pixel so the extremes land at 0.5 and N-0.5 and no clipping test is needed. Under
''      TRUNCATION that reasoning holds - but the code was rounding, and round(N-0.5) is N, one past
''      the last column. So the version that claimed to make clipping unnecessary was the one that
''      actually needed it. The comment asserting the guarantee was written before the guarantee was
''      checked.
''
'' With RAD = N/4 the extreme |Z| = 2 does map to pixel N. It requires both phasors exactly aligned,
'' which is measure-zero, and our graphics layer drops an out-of-range PSet (MODERN semantics) - the
'' same outcome the reference gets from its explicit np.clip. Stated, not assumed.
Const CX  = N / 2.0
Const CY  = N / 2.0
Const RAD = N / 4.0

'' ---- colour ----------------------------------------------------------------------------------
'' Markowski's colouring, kept exactly: red rises with the SPIRAL index, green with the POINT index,
'' and blue is the bitwise complement of their normalised sum. The complement is what stops the
'' figure washing out: where red and green are both high blue is low, so the bright region stays
'' yellow rather than going white, and the arms keep their separation at the rim.
''
'' All three depend on loop counters ONLY - never on the geometry - so they are tabulated here once
'' rather than recomputed 62 500 times per frame. Each entry is pre-shifted into its own byte of the
'' 32-bit pixel, so the inner loop assembles a colour with two ORs and no arithmetic at all.
Dim Shared As Integer gRed(0 To M - 1)            '' [i]     already shifted into bits 16..23
Dim Shared As Integer gGrn(0 To NPTS - 1)         '' [j]     already shifted into bits 8..15
Dim Shared As Integer gBlu(0 To M + NPTS - 2)     '' [i + j] bits 0..7, no shift needed

Sub BuildPalette()
  '' The divisors are (count - 1) so that the LAST index maps to exactly 255 and the full range is
  '' used. Markowski makes the same point in spirals.py: dividing by the count instead would leave
  '' the top of the range unused, which is invisible at M = 250 but obvious at M = 10.
  Dim As Integer mDiv = M - 1
  Dim As Integer nDiv = NPTS - 1
  If mDiv < 1 Then mDiv = 1      '' guard the degenerate single-spiral / single-point cases
  If nDiv < 1 Then nDiv = 1

  For i As Integer = 0 To M - 1
    gRed(i) = (255 * i \ mDiv) * 65536          '' * 65536 is a shift by 16, written as a multiply
  Next i                                        '' because it is a compile-time constant either way
  For j As Integer = 0 To NPTS - 1
    gGrn(j) = (255 * j \ nDiv) * 256
  Next j
  For s As Integer = 0 To M + NPTS - 2
    '' The original writes this as ~(255*(i+j)/(mDiv+nDiv)) & 0xff. For a value already inside
    '' 0..255 the complement is simply 255 - x, which is what is written here: same result, and it
    '' does not depend on the width of the integer type doing the complement.
    gBlu(s) = 255 - (255 * s \ (mDiv + nDiv))
  Next s
End Sub

'' ---- one frame ---------------------------------------------------------------------------------
Sub DrawFrame( ByVal t As Double )
  '' Both loops are written out here rather than factored into a per-spiral SUB, and that is a
  '' measured decision, not a stylistic one. A call per POINT would be the single most expensive
  '' thing in this demo (62 500 calls per frame). A call per SPIRAL would be cheap enough - but it
  '' would put the spiral's state into a procedure-local array, and this loop does not need storage
  '' at all: a spiral carries its two phases in scalars from one step to the next. Introducing an
  '' array to hold what fits in two registers is how a fast loop becomes a slow one.
  Dim As Double phiStep = TWOPI / M

  For i As Integer = 0 To M - 1
    '' The spiral's two CARRIER phases. These never change during the walk - only a and b do - and
    '' that is exactly the property that keeps the figure bounded (see THE RECURRENCE, point 2).
    Dim As Double col = i
    Dim As Double phi = t + phiStep * i

    Dim As Double a = col       '' the walk starts ON the carriers, i.e. with zero feedback
    Dim As Double b = phi

    Dim As Integer red = gRed(i)   '' hoisted: constant for the whole inner loop
    Dim As Integer ij  = i         '' i + j, stepped by one per point instead of recomputed

    For j As Integer = 0 To NPTS - 1
      '' The point: two unit phasors summed. This is the only trigonometry in the demo and it is
      '' about 85% of the frame's cost - four libm calls per point. Everything else in this loop is
      '' noise beside it, which is why the palette is tabulated and the geometry needs no clipping:
      '' the aim is not to make these lines cheap, it is to add nothing to them.
      Dim As Double zr = Cos(a) + Cos(b)
      Dim As Double zi = Sin(a) + Sin(b)

      '' Plot, colour assembled from three pre-shifted table entries with two ORs and no arithmetic.
      '' ⛔ The Int() calls are load-bearing: without them MODERN rounds where the reference truncates
      '' and half the pixels land one place over. See the note at RAD.
      PSet ( Int(CX + RAD * zr), Int(CY + RAD * zi) ), red Or gGrn(j) Or gBlu(ij)

      '' Feed the point back into the phases - from the CARRIERS, never from a and b themselves.
      a = col + K * zr
      b = phi + K * zi
      ij += 1
    Next j
  Next i
End Sub

'' ---- still capture -----------------------------------------------------------------------------
'' Reads the screen back a pixel at a time with POINT and writes a binary PPM (P6). Slow - 160 000
'' POINT calls - but it runs once and only when asked, and it is the only way to look at the figure
'' on a machine with no window.
''
'' ⛔⛔ A STILL IS BLIND TO EVERYTHING THAT HAPPENS BETWEEN FRAMES. Flicker, a figure that drifts off
'' centre over a minute, a feedback term that slowly diverges - none of that shows in one frame, and
'' all of it has bitten this project before. A still is a sanity check that the maths is not garbage.
'' It is never a sign-off on a visual phase: for that, render the video.
Sub DumpFrame( ByVal fname As String )
  Dim As Integer fh = FreeFile
  Open fname For Binary Access Write As #fh
  Dim As String hdr = "P6" + Chr(10) + Str(N) + " " + Str(N) + Chr(10) + "255" + Chr(10)
  Put #fh, , hdr
  Dim As String row
  For y As Integer = 0 To N - 1
    row = ""
    For x As Integer = 0 To N - 1
      Dim As Integer c = Point(x, y)
      row += Chr((c Shr 16) And 255) + Chr((c Shr 8) And 255) + Chr(c And 255)
    Next x
    Put #fh, , row
  Next y
  Close #fh
End Sub

'' ---- main --------------------------------------------------------------------------------------
Dim As Integer maxFrames = 0            '' 0 = run until a key is pressed
If Len(Command(1)) > 0 Then maxFrames = CInt(Command(1))
Dim As String dumpName = Command(2)     '' non-empty = write the LAST frame as a PPM and exit

BuildPalette()
ScreenRes N, N, 32

'' t starts wherever the clock happens to be. The figure has no privileged phase, and seeding it to a
'' constant would make every run of the demo open on the same picture - which looks like a still.
'' A third argument overrides it with a FIXED phase, which is what makes the render reproducible and
'' therefore comparable, pixel for pixel, against the reference implementation. That comparison is
'' the only thing that distinguishes "the figure looks plausible" from "the translation is correct".
Dim As Double t = Timer - Int(Timer)
If Len(Command(3)) > 0 Then t = Val(Command(3))

Dim As Double tStart = Timer
Dim As Double tPrev  = tStart
Dim As Double fps    = 0.0
Dim As Double worst  = 0.0
Dim As Integer frame = 0

Do
  Dim As Double t0 = Timer

  '' ScreenLock/ScreenUnlock brackets the whole frame so the display is updated once, not 62 500
  '' times. Without it the demo is not slower - it TEARS, and tearing is one of the defects a still
  '' cannot show.
  ScreenLock
  '' ⛔ NOT Cls. In this dialect Cls clears to the CURRENT background colour, and in a Commodore-
  '' derived BASIC that default is blue - which is exactly what the first render of this demo showed:
  '' a blue disc on a blue field. The reference builds its frame over a zeroed buffer, i.e. BLACK, and
  '' black is also what makes the colours read: every hue here is at full saturation, and on blue the
  '' low-red arms vanish. Painting the rectangle explicitly costs one filled Line per frame.
  Line (0, 0)-(N - 1, N - 1), 0, BF
  DrawFrame(t)
  '' The readout is drawn AFTER the figure and not before: PRINT inside a graphics mode paints its
  '' own background, so anything already drawn underneath it is erased. Drawn first, it would simply
  '' disappear under the next Cls-and-draw.
  '' It is also suppressed for the first few frames, while the smoothed average is still meaningless.
  If frame > 8 Then
    Locate 1, 1
    Print Using "###.# fps   worst ##.# ms"; fps; worst
  End If
  ScreenUnlock

  Dim As Double t1 = Timer
  Dim As Double dt = t1 - tPrev          '' wall time this frame really took, including the display
  tPrev = t1

  '' An exponential moving average, not an instantaneous 1/dt: the raw value jitters by tens of fps
  '' between consecutive frames and is unreadable on screen. 0.9/0.1 settles in roughly 20 frames.
  If dt > 0 Then fps = fps * 0.9 + (1.0 / dt) * 0.1

  '' The worst frame is the number that says whether the demo will stutter; the average hides it.
  '' The first frames are excluded because they include one-time costs (screen setup, first touch of
  '' the palette tables) that will never happen again.
  Dim As Double ms = (t1 - t0) * 1000.0
  If frame > 8 And ms > worst Then worst = ms

  '' Advance the animation by REAL elapsed time - see PHASE_RATE.
  t += PHASE_RATE * dt

  frame += 1
  If maxFrames > 0 And frame >= maxFrames Then Exit Do
  If Len(Inkey) > 0 Then Exit Do
Loop

If Len(dumpName) > 0 Then DumpFrame(dumpName)

Dim As Double elapsed = Timer - tStart
Print
Print Using "frames ##### in ####.## s  ->  ###.# fps average"; frame; elapsed; frame / elapsed
Print Using "points per frame ###### , worst frame ##.# ms"; M * NPTS; worst
