'' =====================================================================================
''  VOXEL LANDSCAPE  --  a heightmap renderer with no polygons, no GPU, and no z-buffer
''  SedaiBasic demo. Runs on FreeBASIC (fbc) and on SedaiBasic (sb) from the same source.
'' =====================================================================================
''
''  WHAT IT DRAWS, AND WHY THIS WAY
''
''  A heightmap is a grid of altitudes. To turn one into a picture you can build polygons
''  and hand them to a rasteriser -- or you can notice that a landscape seen from above
''  has a very convenient property: from the camera's point of view, WHAT IS NEARER CAN
''  ONLY EVER HIDE WHAT IS FURTHER. Nothing behind you, nothing above you, no re-ordering.
''
''  That property is enough to draw the whole scene with one integer per screen column.
''
''  For each screen column we send a ray out across the map and walk it from
''  the camera outwards. Each sample gives an altitude; we project it to a screen row and
''  paint the vertical span it REVEALS -- the part of that column not already covered by
''  something nearer. A single array, the y-buffer, remembers how far down each column has
''  been filled. When a column is full we stop walking it.
''
''  The result: every screen pixel is written exactly once, and the cost is
''  "samples + pixels" rather than "samples x pixels". See RenderFrame for the details.
''
''  RUNNING IT
''    fbc voxel_landscape.bas && ./voxel_landscape      '' native
''    sb --window voxel_landscape.bas                   '' SedaiBasic, needs a -Window build
''  It walks one full camera circle, prints a frame-time report, and exits. There is no
''  user input on purpose: a fixed path makes two runs comparable.

'' ---- THE TWO MODES -------------------------------------------------------------------
'' VIDEO 0 : draw on screen, in real time. VIDEO 1 : write raw rgb24 frames to a file for
'' ffmpeg, with no screen at all.
''
'' They are one renderer with two SINKS, not two programs, because the moment they became
'' two files they would start to disagree about what they draw. What differs is a single
'' routine, PaintSpan, and the resolution.
''
'' The resolutions are tied to the mode on purpose, because the two modes are gated on
'' different things. Real time is gated on the FRAME: measured here, 640x480 costs 11 ms
'' under the AOT - about 90 fps, with room to spare over 60 - while 1920x1080 costs 54 ms,
'' which is watchable but not fluid. Offline is gated on the WHOLE RUN, and 600 frames at
'' 54 ms is 33 seconds of rendering for 20 seconds of video: the frame time stops being a
'' constraint the moment nobody is waiting for it.
''
''   fbc voxel_landscape.bas && ./voxel_landscape
''   ffmpeg -f rawvideo -pix_fmt rgb24 -s 1920x1080 -r 30 -i frames.raw out.mp4
''
'' Measured, so the offline cost is not a surprise: a 1080p frame takes about 100 ms to fill
'' and write against 54 ms to draw on screen - the byte-at-a-time array writes cost about twice
'' what Line does, which is the price of a format that cannot be got wrong. A full 600-frame
'' circle renders in a minute, and ⚠️ leaves 3.7 GB of raw file: pipe it or delete it after.
''
'' What the three enrichments below cost, measured A/B against the version without them:
'' the colour bands and the sun shadows cost NOTHING per frame - both fold into a colormap
'' that was already being built - and the fog costs about 1 ms of the 640x480 frame, which is
'' the only part of the three that depends on where the camera is.
'' The mode is chosen at RUN TIME, from the command line, so one build does both:
''
''   voxel_landscape            real time on screen, 640x480   (the default)
''   voxel_landscape video      raw rgb24 frames to a file, 1920x1080, no screen
''
'' ⭐ In WebAssembly the offline mode excludes ITSELF, with no special case: a module has
'' no command line, so COMMAND$ answers the empty string and the default branch is taken.
'' Writing a 3.7 GB file from a browser tab was never going to be the point.
''
'' ⚠️ SCRW and SCRH were Const until the mode became a run-time choice, and constants are
'' worth something here: the compiler folds them into the renderer's arithmetic. They are
'' now variables, and that trade was measured rather than assumed - see the note by the
'' frame-time report.
Dim Shared As Integer VIDEO
Dim Shared As Integer SCRW, SCRH

VIDEO = 0
#if __SB_WASM__
    '' ⛔ The offline mode is compiled OUT for WebAssembly, and a run-time test would not
    '' have been enough: the backend refuses an uncovered opcode for being PRESENT in the
    '' program, not for being reached, so the file-writing branch has to be absent from
    '' the source the backend sees. A browser tab has no file to write anyway.
#else
    If Len(Command$(1)) > 0 Then
        If LCase(Command$(1)) = "video" Then VIDEO = 1
    End If
#endif

If VIDEO Then
    SCRW = 1920 : SCRH = 1080
Else
    SCRW = 640                 '' the crossover point, measured: at this size the traversal
    SCRH = 480                 ''   and the span filling cost about the same (52% / 48%)
End If
Dim Shared As Integer HORIZON
HORIZON = SCRH * 2 \ 5 '' screen row of the eye line. Below centre, so more of the
                               ''   picture is ground than sky - the ground is the subject.
Const MAPSZ     = 256          '' power of two so wrapping is an AND, not a modulo. Small
Const MAPMASK   = MAPSZ - 1    ''   enough that world generation takes well under a second
                               ''   interpreted; the camera circle never reveals the repeat.
Const SEED      = 20260806     '' fixed: two runs must produce the same terrain, or the
                               ''   frame times below would not be comparable.
Const ZNEAR     = 5.0          '' first sample distance. ⚠️ MEASURED, not guessed: an instrumented
                               ''   run over the whole camera circle reported that the nearest
                               ''   distance at which any column ever paints is 7.87, so starting
                               ''   at 1 spent a third of the walk on ground that is always below
                               ''   the bottom of the screen. 5.0 keeps a margin over the measured
                               ''   minimum without paying for the part nothing can ever see.
Const ZFAR      = 220.0        '' where the world ends and the sky begins. Found by walking
                               ''   the camera and raising it until no more detail appeared:
                               ''   past ~220 the terrain is under a pixel tall.
Const SPACING   = 1.5          '' rows between consecutive samples - see BuildSteps. The whole
                               ''   step law is expressed in SCREEN ROWS, which is the unit the
                               ''   defect it fixes was measured in.
Const NEARROWS  = 5.0          '' row budget for the APPROACH, where the far rule does not apply.
                               '' ⚠️ The approach was originally a flat 1.08 growth on the argument
                               ''   that nothing there can be on screen. That is true only of ground
                               ''   BELOW the eye. Terrain at or near eye level projects close to the
                               ''   horizon at ANY distance, so it is on screen from the very first
                               ''   sample - and 1.08 draws it in bands FORTY-EIGHT ROWS tall.
                               ''   Worked out: for a geometric step the worst row jump anything on
                               ''   screen can make is (SCRH-HORIZON) * (1 - 1/g), which does not
                               ''   depend on the distance at all. So the approach is budgeted the
                               ''   same way as the far region - in rows - and g follows from it.
                               ''   5 rather than SPACING because this bounds the WORST case, which
                               ''   is rare, where SPACING bounds the case that is always present.
Const ZTABMAX   = 1024         '' ceiling on the sample count; the table comes out at ~350 at
                               ''   1080p and ~175 at 640x480.
Const FOGRES    = 4.0          '' fog table entries per unit of distance - see BuildFog.
Dim Shared As Double VSCALE
VSCALE = 0.75 * SCRH   '' vertical exaggeration, as a fraction of the screen height
                               ''   so the framing does not change with the resolution.
                               ''   Empirical at 200 rows: 100 looked like a pancake, 250
                               ''   turned every slope into a cliff; 150 = 0.75 * 200.
Const EYECLEAR     = 45.0         '' how far the eye rides ABOVE the ground under it. Not
                               ''   named CLEAR: that is a FreeBASIC keyword (the memset).
                               '' ⚠️ This replaced a fixed eye altitude, and the instrument
                               '' is what said so. With the camera pinned at 165 units the
                               '' step counter reported the SAME 144960 samples on almost
                               '' every frame - one column x 453 steps for every column,
                               '' walking the full distance and the y-buffer's early exit
                               '' never firing once. The renderer was correct and the
                               '' occlusion property this file spends a page explaining was
                               '' doing nothing, because from that height no ridge ever
                               '' reached the top of the screen. Three frames were worse
                               '' still: one sample per column and a blank picture, having
                               '' flown straight through a peak taller than it was.
                               '' Riding the terrain fixes both: near ridges now fill their
                               '' columns, columns exit early, and the eye can never be
                               '' inside a hill.
Const CAMR      = 100.0        '' radius of the camera circle, in map cells. Large, because the
                               ''   radius and the lap time together ARE the turn rate, and the turn
                               ''   rate is what a viewer feels - see the camera path below.
Const EYELAG    = 0.06         '' how tightly the eye follows the ground under it, per frame.
Const FRAMES    = 1800         '' 60 seconds at 30 fps. The run ends here so it can be measured.
Const STATN     = 120          '' frame-time window the on-screen counter reports on.

'' ⚠️ ALTITUDE IS A FLOAT, AND IT HAS TO BE. This was a UByte, which is the obvious choice for
'' a heightmap and is what put the last of the blockiness in the picture. The terrain rises less
'' than one whole unit per cell over any gentle slope, so rounding to integers turns those slopes
'' into plateaus several cells wide separated by one-unit steps - and near the camera ONE UNIT OF
'' ALTITUDE IS FOURTEEN PIXELS TALL (VSCALE/z with z at the near edge of the screen). Bilinear
'' interpolation cannot put back a difference that was never stored.
'' ⭐ The diagnosis came from an experiment that made things WORSE. Suspecting the cell size, the
'' map was doubled to 512 with the horizontal and vertical scales doubled to match - and a fine
'' quilted texture appeared over the whole landscape. Doubling VSCALE had doubled the height of
'' one altitude unit on screen, which is the opposite of what a cell-size problem would do. The
'' experiment that fails in an informative direction is worth more than the one that works.
Dim Shared As Single   hmap(MAPSZ * MAPSZ - 1)   '' altitude, 0..255, fractional
'' The surface colour is stored as three separate byte planes, not as the packed integer that
'' RGB() returns, and the reason is portability. RGB() packs its channels into an integer whose
'' BYTE ORDER is the compiler's business, not ours - a raw file written from packed values comes
'' out with red and blue swapped on one of the two targets, and nobody notices until the video
'' is watched. A byte plane has a meaning that is written down; a packed integer does not.
'' There WAS a fourth, packed array here, so that Line could be handed a colour with no work per
'' span. Fog and bilinear colour both took that away - a span's colour is now computed rather
'' than looked up - so the packed copy had no reader left, and 256 KB went with it.
Dim Shared As UByte    cmR(MAPSZ * MAPSZ - 1)
Dim Shared As UByte    cmG(MAPSZ * MAPSZ - 1)
Dim Shared As UByte    cmB(MAPSZ * MAPSZ - 1)
'' One frame, rgb24, exactly the bytes ffmpeg is told to expect. ⚠️ Allocated ONLY in the
'' offline mode: at 1080p it is 6.2 MB, which a real-time run has no use for.
ReDim Shared As UByte fbuf(0)
If VIDEO Then ReDim fbuf(SCRW * SCRH * 3 - 1)
ReDim Shared As Integer ybuf(SCRW - 1)           '' the occlusion state, one row per column

'' Insertion sort, in place. Used by both the on-screen window and the end-of-run report, so
'' there is one ordering of the samples rather than two that could drift apart. O(n^2), which
'' is the right trade at these sizes: 120 samples sorted every fifteenth frame, and 599 once.
Sub SortAsc(s() As Double, ByVal n As Integer)
    Dim As Integer i, j
    Dim As Double k
    For i = 1 To n - 1
        k = s(i) : j = i - 1
        While j >= 0 AndAlso s(j) > k
            s(j + 1) = s(j) : j -= 1
        Wend
        s(j + 1) = k
    Next
End Sub

'' -------------------------------------------------------------------------------------
''  WORLD GENERATION
''
''  Value noise summed over octaves, not diamond-square, and the reason is the camera:
''  it flies a closed circle for ever, so the map has to REPEAT WITHOUT A SEAM. Value
''  noise on a power-of-two lattice tiles for free - the lattice index is masked, so the
''  right edge and the left edge read the same corner. Diamond-square wants a (2^n)+1
''  grid and its wrap has to be built by hand. It is also about half the code, which
''  matters when the comments are the point, and its determinism is easier to defend:
''  it depends on a hash of the coordinate, not on the ORDER random numbers were drawn.
'' -------------------------------------------------------------------------------------

'' A hash of a lattice point to [0,1). Masked back to 31 bits after every multiply so it
'' cannot depend on how a particular compiler handles integer overflow - the two we target
'' must agree on the terrain, or the frame times are measured on different worlds.
Function Hash01(ByVal ix As Integer, ByVal iy As Integer) As Double
    Dim As Integer h = (ix * 73856093) Xor (iy * 19349663) Xor (SEED * 83492791)
    h = h And &h7FFFFFFF
    h = (h Xor (h Shr 13)) * 1274126177
    h = h And &h7FFFFFFF
    Return h / 2147483647.0
End Function

'' One octave: bilinear interpolation between four lattice corners, with a smoothstep on
'' the fractions. Straight linear interpolation leaves visible creases along the lattice
'' lines - the value is continuous but its slope is not, and the eye finds the grid.
Function ValueNoise(ByVal x As Double, ByVal y As Double, ByVal freq As Integer) As Double
    Dim As Double fx = x * freq / MAPSZ, fy = y * freq / MAPSZ
    Dim As Integer ix = Int(fx), iy = Int(fy)
    Dim As Double tx = fx - ix, ty = fy - iy
    tx = tx * tx * (3.0 - 2.0 * tx)          '' smoothstep
    ty = ty * ty * (3.0 - 2.0 * ty)
    Dim As Integer x0 = ix And (freq - 1), x1 = (ix + 1) And (freq - 1)
    Dim As Integer y0 = iy And (freq - 1), y1 = (iy + 1) And (freq - 1)
    Dim As Double a = Hash01(x0, y0), b = Hash01(x1, y0)
    Dim As Double c = Hash01(x0, y1), d = Hash01(x1, y1)
    Return (a + (b - a) * tx) + ((c + (d - c) * tx) - (a + (b - a) * tx)) * ty
End Function

'' -------------------------------------------------------------------------------------
''  COLOUR BY ALTITUDE BAND  (phase 2, step 1)
''
''  Phase 1 painted a single green ramp, and looking at a rendered frame is what said it was
''  wrong: the landscape read as one flat sheet of green with the shapes barely legible. Real
''  ground changes MATERIAL with height, not just brightness - water, sand, grass, rock, snow -
''  and it is the material boundaries that let the eye read a slope.
''
''  ⚠️ THE FIRST ATTEMPT AT THIS TABLE FAILED, AND THE REASON IS WORTH MORE THAN THE TABLE.
''  The stops were spread over the nominal 0..255 of a UByte. The picture came back green and
''  sand-coloured with no water, no rock and no snow, so instead of nudging colours I counted
''  what the generator actually produces: altitudes run 27..211, and HALF the map lies between
''  93 and 137 - a 44-unit window out of 256. Four octaves of noise averaged together pull hard
''  toward the mean; the extremes need all four octaves to agree, which is rare. So a stop at
''  200 was above the 95th percentile and one at 70 was below the 5th: two of five bands could
''  not be reached by any point on the map. The stops below are placed on the MEASURED
''  distribution, and that is the only way they could have been right.
''
''  Bands are FLAT with a narrow blended edge, not a ramp from stop to stop. A ramp is what
''  phase 1 already was, only in more colours; a flat band is what makes a material read as a
''  material. But a hard cut draws a contour line along every hillside at exactly the band
''  height and the eye reads those lines as terraces that are not there - so each boundary is
''  blended over ±BLENDW altitude units with a smoothstep. BLENDW = 5 because the narrowest
''  band here (the shoreline) is 12 units wide: any wider and two blend windows would overlap
''  and the shore would stop being a colour of its own.
Const NBANDS    = 6
Const BLENDW    = 5.0

'' Lower edge of each band, plus a sentinel above the highest altitude. Percentiles of the
'' generated map, for reference: p05=67  p25=93  p50=114  p75=137  p95=171, range 27..211.
Dim Shared As Integer sh(0 To NBANDS) = {   0,  64,  76,  88, 155, 182, 999 }
Dim Shared As Integer sr(0 To NBANDS) = {  22,  52, 200,  76, 110, 246,   0 }
Dim Shared As Integer sg(0 To NBANDS) = {  52, 108, 190, 126, 108, 248,   0 }
Dim Shared As Integer sb(0 To NBANDS) = { 104, 158, 138,  58,  98, 255,   0 }
''                                       deep  sea  sand grass rock snow  (sentinel)

'' ⭐ All of it happens ONCE, here, into the colormap. The renderer reads a colour it never
'' computes - the same trade the whole demo is built on, and the reason the measured frame
'' time does not move at all when this is switched on.
'' ⚠️ These tables live at module level rather than as Static arrays inside the Sub because
'' "Static a(0 To 5) As Integer = {...}" inside a procedure is accepted by fbc and REJECTED by
'' this implementation - a real gap, logged, but not one to trip over in a demo.
Sub BandColour(ByVal h As Double, ByRef r As Integer, ByRef g As Integer, ByRef b As Integer)
    Dim As Integer i = 0
    While i < NBANDS - 1 AndAlso h >= sh(i + 1)
        i += 1
    Wend

    '' Default is the flat band colour; only a point within BLENDW of a boundary mixes. Both
    '' branches use the same formula - t runs 0 at (edge - BLENDW) to 1 at (edge + BLENDW),
    '' so it is 0.5 exactly on the edge and the two sides of a boundary agree.
    Dim As Integer lo = i, hi = i
    Dim As Double  t = 0.0
    If i < NBANDS - 1 AndAlso h > sh(i + 1) - BLENDW Then
        lo = i     : hi = i + 1 : t = (h - (sh(i + 1) - BLENDW)) / (2.0 * BLENDW)
    ElseIf i > 0 AndAlso h < sh(i) + BLENDW Then
        lo = i - 1 : hi = i     : t = (h - (sh(i)     - BLENDW)) / (2.0 * BLENDW)
    End If
    If t < 0.0 Then t = 0.0
    If t > 1.0 Then t = 1.0
    t = t * t * (3.0 - 2.0 * t)     '' smoothstep: zero slope at both ends, so the blend has no
                                    '' visible seam where it meets the flat part of the band

    r = sr(lo) + (sr(hi) - sr(lo)) * t
    g = sg(lo) + (sg(hi) - sg(lo)) * t
    b = sb(lo) + (sb(hi) - sb(lo)) * t
End Sub

Sub BuildWorld()
    Dim As Integer x, y, br, bg, bb
    Dim As Double  h
    Dim As Double n, amp, total
    For y = 0 To MAPSZ - 1
        For x = 0 To MAPSZ - 1
            '' Four octaves, each twice the frequency and half the amplitude. Fewer than
            '' four and the hills have no small detail to catch the light; more than four
            '' and the extra octaves are finer than one map cell, so they cost generation
            '' time and produce nothing a sample can ever see.
            n = 0.0 : amp = 1.0 : total = 0.0
            n += ValueNoise(x, y, 4)  * amp : total += amp : amp *= 0.5
            n += ValueNoise(x, y, 8)  * amp : total += amp : amp *= 0.5
            n += ValueNoise(x, y, 16) * amp : total += amp : amp *= 0.5
            n += ValueNoise(x, y, 32) * amp : total += amp
            n = n / total                                  '' back to 0..1
            h = n * 255.0
            If h < 0.0 Then h = 0.0
            If h > 255.0 Then h = 255.0
            hmap(y * MAPSZ + x) = h
        Next
    Next
End Sub

'' -------------------------------------------------------------------------------------
''  SUN SHADOWS  (phase 2, step 2)
''
''  ⭐ THE TRADEOFF THE BRIEF ASKS TO BE EXPLICIT ABOUT: this could be done per frame or once.
''  Per frame it is a shadow test for every sample the traversal touches - roughly 150 000 of
''  them at 640x480 - and it would have to be redone every frame even though NOTHING MOVES:
''  the sun is fixed and so is the ground, so the answer is the same on frame 1 and frame 600.
''  Precomputing it costs one sweep of the map (2 x 65 536 steps, well under a millisecond) and
''  ZERO bytes of extra storage, because the colormap is already indexed per CELL rather than
''  per altitude - so the light simply multiplies into a colour that was going to be stored
''  anyway. The renderer is not told that shadows exist. That is the whole trick, and it is the
''  same trick as the colour bands: anything that depends only on WHERE a cell is, and not on
''  where the camera is, belongs in the colormap.
''  ⚠️ What it buys is also what it costs: the sun can never move. A day/night cycle would mean
''  rebuilding the colormap, which at this map size is a few milliseconds - fine once a second,
''  not fine once a frame.
''
''  The sweep itself: walk the map in the direction the LIGHT travels, carrying the height of
''  the light ray. Each step the ray drops by LSLOPE. If the ground reaches the ray, this cell
''  is lit and it becomes the new ray height - it is the thing casting from here on. If the
''  ground is below the ray, something upwind is blocking the sun and the cell is in shadow.
''  One pass over the map answers every cell, which is why this is a sweep and not a per-cell
''  search: the expensive question ("what is between me and the sun?") is answered incrementally
''  by the cell before me.
''
''  ⚠️ The map is a torus, so a sweep line has no beginning - and the first few cells of any
''  start point would be wrong, because the ray arrives carrying no history. The fix is to walk
''  TWICE round and only record the second lap; by then the ray height is whatever the terrain
''  actually dictates. Doubling a sub-millisecond pass is the cheapest correctness there is.
Const LSLOPE    = 1.15   '' height units the sun ray falls per cell step - i.e. the sun's elevation.
                         '' Found by looking: at 2.0 the sun is so high almost nothing is shadowed,
                         '' at 0.6 whole valleys go dark and the bands stop reading. 1.15 is a
                         '' late-afternoon sun - shadows long enough to model the ridges, short
                         '' enough that they stay attached to what casts them.
Const AMBIENT   = 0.42   '' how much light a cell gets with no sun at all - in shadow, or facing
                         '' away. Not zero: outdoors the sky is a second light source, and a black
                         '' shadow reads as a hole in the ground rather than as shade. Applied in
                         '' PaintWorld, where the sun's two terms are combined, not in the sweep.
Const PENUMB    = 9.0    '' height units over which shadow deepens to full. A hard edge betrays the
                         '' grid - the shadow boundary comes out staircased along the cell lattice.
                         '' Fading over a few units hides the lattice without softening the shape.

Dim Shared As UByte lit(MAPSZ * MAPSZ - 1)   '' 0..255, how much of the sun each cell sees

Sub CastShadows()
    Dim As Integer d, k, x, y, c
    Dim As Double  h
    Dim As Double  ray, f, depth
    For d = 0 To MAPSZ - 1
        '' One diagonal per starting column. A (+1,+1) walk on a power-of-two torus closes
        '' after exactly MAPSZ steps, so MAPSZ diagonals cover every cell once and none twice.
        x = d : y = 0
        ray = -1000.0                       '' arbitrary: the priming lap overwrites it
        For k = 0 To 2 * MAPSZ - 1
            c = y * MAPSZ + x
            h = hmap(c)
            ray -= LSLOPE
            If h >= ray Then
                ray = h                     '' lit, and from here on THIS is what casts
                f = 1.0
            Else
                depth = ray - h
                f = depth / PENUMB
                If f > 1.0 Then f = 1.0
                f = 1.0 - f                 '' 1 = sun reaches it, 0 = fully blocked
            End If
            If k >= MAPSZ Then lit(c) = Int(f * 255.0)   '' second lap only
            x = (x + 1) And MAPMASK
            y = (y + 1) And MAPMASK
        Next
    Next
End Sub

'' Material and light meet here, and only here. Kept separate from BuildWorld because the
'' shadow sweep needs the COMPLETE heightmap: a cell's shade depends on ground the generator
'' has not reached yet when that cell's own height is written.
'' -------------------------------------------------------------------------------------
''  ⚠️ THE BANDS AND THE CAST SHADOWS TOGETHER ARE NOT ENOUGH, and the reason is worth having.
''  Inside a band the colour is deliberately FLAT, and a cast shadow is very nearly binary: a
''  cell either sees the sun or it does not. So over a lit hillside the ground has exactly ONE
''  colour, and every visible variation is a boundary - a band edge or a shadow edge. The eye
''  finds those boundaries and reads them as CONTOUR LINES. The picture is smooth and it looks
''  banded, which is the opposite of the intent.
''
''  What is missing is the term that varies CONTINUOUSLY: how much a piece of ground is TILTED
''  toward the sun. A slope facing the light is bright, one facing away is dim, and between
''  them is every value - so the flat interior of a band stops being flat without the band
''  itself being weakened.
''
''  The surface normal comes from a central difference of the heightmap. Central rather than
''  forward because a forward difference measures the slope half a cell away from where it is
''  used, which shifts the whole shading half a cell downhill and puts a bright fringe on one
''  side of every ridge. The 2.0 in the z component is the two-cell baseline the central
''  difference spans, not a tuning constant: get it wrong and the terrain reads as steeper or
''  flatter than it is drawn.
''
''  Two terms, one sun: the cast shadow says whether the sun is VISIBLE from here, the diffuse
''  term says how squarely it lands. They multiply. Ambient is added afterwards so that neither
''  can drive a cell to black.
Sub PaintWorld()
    Dim As Integer c, x, y, br, bg, bb
    Dim As Double  f, nx, ny, nz, ilen, dif
    Dim As Double  lx = -1.0, ly = -1.0, lz = LSLOPE
    ilen = 1.0 / Sqr(lx * lx + ly * ly + lz * lz)
    lx *= ilen : ly *= ilen : lz *= ilen

    For c = 0 To MAPSZ * MAPSZ - 1
        x = c And MAPMASK
        y = c \ MAPSZ
        BandColour(hmap(c), br, bg, bb)

        nx = -(hmap(y * MAPSZ + ((x + 1) And MAPMASK)) - hmap(y * MAPSZ + ((x - 1) And MAPMASK)))
        ny = -(hmap(((y + 1) And MAPMASK) * MAPSZ + x) - hmap(((y - 1) And MAPMASK) * MAPSZ + x))
        nz = 2.0
        ilen = 1.0 / Sqr(nx * nx + ny * ny + nz * nz)
        dif = (nx * lx + ny * ly + nz * lz) * ilen
        If dif < 0.0 Then dif = 0.0

        f = AMBIENT + (1.0 - AMBIENT) * dif * (lit(c) / 255.0)
        br = Int(br * f) : bg = Int(bg * f) : bb = Int(bb * f)
        cmR(c) = br
        cmG(c) = bg
        cmB(c) = bb
    Next
End Sub

'' -------------------------------------------------------------------------------------
''  WHERE TO SAMPLE, and this is the third answer to the question - the first two were wrong in
''  ways worth keeping, because both were invisible until something else got fixed.
''
''  Attempt one was a geometric step, with a comment claiming it kept the sample spacing roughly
''  constant in SCREEN space. Work it out: consecutive samples land
''      dh * VSCALE * (1/z - 1/z') rows apart,
''  which falls as 1/z. Not constant - WIDEST NEAREST. At the original settings that came to 7.7
''  rows at the bottom of a 1080p screen against 2.0 at the horizon, so the near ground was the
''  WORST-sampled part of the picture. Nobody noticed while the terrain was still a staircase of
''  flat cells, because the staircase was coarser than the sampling. ⭐ AN EMPIRICAL CONSTANT
''  FOUND IN THE PRESENCE OF A BIGGER DEFECT IS ONLY VALID WHILE THAT DEFECT IS THERE.
''
''  Attempt two took two regimes - big steps while the sample was off the bottom of the screen,
''  small ones once it was on - and tested which regime it was in using ytop, which was already
''  computed. The horizontal banding went. VERTICAL banding arrived: ytop depends on the ground
''  under THAT COLUMN, so neighbouring columns left the coarse regime at distances up to 8% apart,
''  and 8% of z near the bottom of the screen is many rows. ⭐ A PER-COLUMN DECISION MAKES A
''  PER-COLUMN SEAM. The sampling has to be identical in every column, or the columns show.
''
''  So: build the distances ONCE, before any frame, and walk the same table in every column. And
''  build it BACKWARDS - not by choosing distances and seeing where they land, but by choosing
''  the rows and inverting the projection to get the distance:
''      row = HORIZON + EYECLEAR * VSCALE / z   ->   z = EYECLEAR * VSCALE / (row - HORIZON)
''  Step the row down by SPACING and the sample spacing on screen is SPACING BY CONSTRUCTION,
''  everywhere, with no constant to tune and nothing to re-tune when the resolution changes.
''  EYECLEAR is the reference height because it is the height difference to the ground directly
''  under the camera - the common case, and the one that fills the bottom of the screen.
''
''  The approach in front of that is a different problem: below the distance where even the
''  reference ground is off the bottom of the screen, nothing can be seen at all, and an
''  instrumented run over the whole camera path confirmed that no column ever paints closer than
''  z = 7.87. Those distances are covered geometrically and coarsely - the point is to leave the
''  region, not to sample it. Of the original 453 steps, 340 were in exactly this dead zone.
Dim Shared As Double  ztab(0 To ZTABMAX - 1)
Dim Shared As Integer znum

Sub BuildSteps()
    Dim As Double z = ZNEAR, r
    Dim As Double zon = EYECLEAR * VSCALE / (SCRH - 1 - HORIZON)   '' first distance on screen
    Dim As Double g   = 1.0 / (1.0 - NEARROWS / (SCRH - HORIZON))  '' see NEARROWS

    znum = 0
    Do While z < zon AndAlso znum < ZTABMAX
        ztab(znum) = z : znum += 1
        z = z * g
    Loop

    r = SCRH - 1
    Do While r > HORIZON AndAlso znum < ZTABMAX
        z = EYECLEAR * VSCALE / (r - HORIZON)
        If z >= ZFAR Then Exit Do
        ztab(znum) = z : znum += 1
        r -= SPACING
    Loop
End Sub

'' -------------------------------------------------------------------------------------
''  ATMOSPHERIC FOG  (phase 2, step 3)
''
''  ⚠️ This is the first thing in the demo that CANNOT go in the colormap, and it is worth
''  saying why: the bands depend on where a cell is, the shadows depend on where a cell is and
''  where the sun is - both fixed - but fog depends on how far the cell is FROM THE CAMERA, and
''  the camera moves. It has to be paid per frame. So the question becomes how little can be
''  paid, and the answer is in two parts.
''
''  First: the fog WEIGHT is precomputable after all, because it depends on the distance and on
''  nothing else. A table over DISTANCE, four entries per world unit, answers it with no exp() in
''  the hot loop at all. (An exponential per sample would be about 150 000 transcendental calls a
''  frame at 640x480 - the sort of thing that quietly costs more than the renderer.)
''  ⚠️ It was originally indexed by STEP NUMBER, which was smaller and faster: the walk always
''  started at ZNEAR and always multiplied by ZSTEP, so step k was at the same distance in every
''  column of every frame. The two-regime stepping below broke that - how fast a column advances
''  now depends on the ground under it - and this is the kind of coupling that does not announce
''  itself: nothing fails to compile, the fog simply stops matching the distance. Indexing by the
''  quantity the value actually depends on cannot come apart that way.
''
''  Second: it is applied PER SPAN, not per pixel - three integer multiply-adds for a strip
''  that may be hundreds of pixels tall, and NOT ONE EXTRA PIXEL IS WRITTEN. Fog that darkens
''  what is already being drawn is free in the only currency this renderer spends: the phase-1
''  scaling law says drawing dominates at high resolution, and this adds no drawing.
''
''  The curve is exp(-density * (z/ZFAR)^2), not linear in distance. Linear fog has a visible
''  onset - a plane at a fixed distance where the haze switches on - because its derivative
''  jumps from zero. The squared exponential starts flat, so near ground is untouched, and it
''  approaches full haze asymptotically, so the far ridges dissolve into the sky instead of
''  ending against it. FOGDENS = 2.0 was chosen by rendering both and comparing: with no fog
''  at all the far ridges finish against the sky as a hard cut-out silhouette, and at 3.0 the
''  haze has reached the middle distance and taken the snow cap and the shadow modelling with
''  it - the picture gets more atmosphere and less landscape. 2.0 keeps both.
''
''  ⚠️ AND THAT ALONE IS NOT ENOUGH, WHICH ONLY THE MOVING PICTURE SHOWS. An exponential
''  reaches full haze at infinity; we stop the world at ZFAR. At 2.0 the haze is 86% there,
''  so ground arriving at the clip distance arrives at 14% contrast against the sky - faint,
''  but not nothing. And ground that far away projects to within a pixel or two of the horizon
''  line, so what the eye sees is not a mountain emerging from haze: it is a mountain GROWING
''  UPWARDS OUT OF THE HORIZON as the camera advances. The landscape looks like it is being
''  built rather than approached, and no still frame says a word about it.
''
''  The fix is to stop treating the clip plane as if it were infinity. Past CLIPFADE of the way
''  out, the haze is closed the rest of the way to FULL with a smoothstep, so that whatever
''  crosses ZFAR crosses it painted exactly the colour of the sky it comes out of. Nothing can
''  pop into a picture it is already indistinguishable from. CLIPFADE = 0.55 because the ramp
''  must be long enough to be invisible - shorter and the closing itself becomes a moving band
''  of haze - and short enough to leave the middle distance the depth the exponential gives it.
''
''  ⭐ And the colour fogged TOWARD is exactly the sky colour, not a grey. That is what makes
''  the horizon disappear rather than fade: at maximum distance the ground is painted the same
''  value as the pixels above it, so there is nothing left for the eye to find an edge in.
'' The sky, and therefore the colour everything fades into. Named once because the clear,
'' the video clear and the fog must agree - if they drift, the horizon grows a seam.
Const HORR      = 120
Const HORG      = 160
Const HORB      = 210
Const FOGDENS   = 2.0
Const CLIPFADE  = 0.55   '' fraction of ZFAR at which the haze starts closing to fully opaque
Const FOGSTEPS  = Int(ZFAR * FOGRES) + 2

Dim Shared As Integer fogw(0 To FOGSTEPS - 1)   '' 0..256, the horizon colour's share

Sub BuildFog()
BuildSteps()
    Dim As Integer k
    Dim As Double  z, f
    For k = 0 To FOGSTEPS - 1
        z = k / FOGRES
        Dim As Double u = z / ZFAR
        If u > 1.0 Then u = 1.0
        f = 1.0 - Exp(-FOGDENS * u * u)

        '' close the remaining gap to full haze over the last stretch before the clip plane
        Dim As Double c = (u - CLIPFADE) / (1.0 - CLIPFADE)
        If c > 0.0 Then
            If c > 1.0 Then c = 1.0
            c = c * c * (3.0 - 2.0 * c)
            f = f + (1.0 - f) * c
        End If

        fogw(k) = Int(f * 256.0)
    Next
End Sub

'' -------------------------------------------------------------------------------------
''  SAMPLING THE GROUND BETWEEN CELLS
''
''  THE PICTURE IS WHAT CAUGHT THIS, and it is worth stating plainly: reading the heightmap
''  with Int() alone makes every map cell a FLAT PLATEAU with a vertical wall around it. On a
''  256-cell map with the eye riding close to the ground, one cell covers tens of pixels near
''  the camera, and the landscape comes out as a field of cubes. No timing and no counter says
''  a word about it - it is only visible by looking at a frame.
''
''  The fix is to read the FOUR cells around the sample point and mix them by the fractional
''  part of the position: bilinear interpolation. The heightmap stops being a staircase and
''  becomes a surface, and nothing else in the renderer changes - the projection, the y-buffer
''  and the occlusion test never knew the height came from a grid at all.
''
''  This function is also what the CAMERA rides on, and that mattered more than expected. With
''  the eye height read from a single cell, the camera stepped up and down by whole altitude
''  units as it crossed cell boundaries - about one crossing every one and a half frames on this
''  circle - and the whole frame jolted vertically. The terrain was smooth and the VIEW was not.
''
''  ⛔ AND A SMOOTHSTEP ON THESE WEIGHTS IS A MISTAKE, WHICH IS WORTH SPELLING OUT BECAUSE THERE
''  IS ONE TWENTY LINES AWAY IN ValueNoise THAT IS CORRECT. There, the lattice is a noise lattice
''  and the values on it are arbitrary: making the interpolation flatten at each lattice point is
''  exactly the shaping wanted, and it is what turns blocky value noise into rolling ground. Here
''  the lattice is the MAP and the values on it are already the smooth landscape. Forcing the
''  gradient to zero at every corner turns each cell into a small plateau with a ramp round its
''  edge, and the ridge lines come out visibly RIPPLED at exactly cell frequency - traded a
''  crease for a wobble. Smoothstep is a tool for SHAPING A LATTICE, not for interpolating data
''  that already means something.
''  The crease it was meant to fix is real, but it was fixed in the wrong place: what suffers
''  from it is the camera, and the camera is damped where it is computed. See EYELAG.
''
''  The offset of MAPSZ*4 before the mask is not decoration. The ray reaches negative world
''  coordinates - the camera circles the middle of the map and ZFAR is 220 cells - and Int()
''  truncates TOWARD ZERO, so at wx = -1.3 it returns -1 and the fractional part comes out as
''  -0.3. A negative weight makes the interpolation EXTRAPOLATE, and the terrain grows spikes
''  along the line where the world coordinate crosses zero. Adding four map widths first costs
''  one addition and makes the argument unconditionally positive, so that Int() IS floor.
Function HeightAt(ByVal wx As Double, ByVal wy As Double) As Double
    Dim As Double  px = wx + MAPSZ * 4.0, py = wy + MAPSZ * 4.0
    Dim As Integer x0 = Int(px) And MAPMASK, y0 = Int(py) And MAPMASK
    Dim As Integer x1 = (x0 + 1) And MAPMASK, y1 = (y0 + 1) And MAPMASK
    Dim As Double  tx = px - Int(px), ty = py - Int(py)

    Return (hmap(y0 * MAPSZ + x0) * (1.0 - tx) + hmap(y0 * MAPSZ + x1) * tx) * (1.0 - ty) _
         + (hmap(y1 * MAPSZ + x0) * (1.0 - tx) + hmap(y1 * MAPSZ + x1) * tx) * ty
End Function

'' The one routine that differs between the two modes. Everything above and below it - the
'' world, the camera, the traversal, the y-buffer - is shared, which is the point: the video
'' is of the same renderer, not of a second one that drifted.
'' In VIDEO mode there is no Line and no screen to draw on; the span goes straight into the
'' frame as bytes. That also makes the video build HEADLESS: it needs no display at all,
'' which is how an offline render actually gets run.
Sub PaintSpan(ByVal x As Integer, ByVal y0 As Integer, ByVal y1 As Integer, _
              ByVal cr As Integer, ByVal cg As Integer, ByVal cb As Integer, ByVal fw As Integer)
    '' The one blend, done once for the whole strip. Dividing by 256 rather than by 255 keeps
    '' it in integers; the rounding error is under half a level and no eye has ever found it.
    Dim As Integer r = cr + ((HORR - cr) * fw) \ 256
    Dim As Integer g = cg + ((HORG - cg) * fw) \ 256
    Dim As Integer b = cb + ((HORB - cb) * fw) \ 256
If VIDEO Then
    Dim As Integer i, o
    For i = y0 To y1
        o = (i * SCRW + x) * 3
        fbuf(o)     = r
        fbuf(o + 1) = g
        fbuf(o + 2) = b
    Next
Else
    Line (x, y0)-(x, y1), RGB(r, g, b)
End If
End Sub

'' -------------------------------------------------------------------------------------
''  THE RENDERER
'' -------------------------------------------------------------------------------------
Sub RenderFrame(ByVal camx As Double, ByVal camy As Double, ByVal ang As Double, ByVal camh As Double)
    Dim As Integer x, i, k, ytop, ybot, si
    Dim As Integer mx0, my0, mx1, my1, i00, i10, i01, i11, cr, cg, cb
    Dim As Double  hgt, tx, ty
    Dim As Double dirx, diry, planex, planey, camc, rx, ry, z, wx, wy

    '' The camera basis. "plane" is the view direction turned 90 degrees and scaled by
    '' tan(half the horizontal field of view); a ray is dir + plane * c with c running
    '' -1..+1 across the screen. Built this way the ray's component ALONG dir is exactly
    '' 1, so the distance we march is already the perpendicular depth - no fisheye, and
    '' no per-column cosine to divide it out later.
    dirx = Cos(ang) : diry = Sin(ang)
    planex = -diry * 0.7 : planey = dirx * 0.7      '' 0.7 ~ 70 degrees horizontal FOV

    For x = 0 To SCRW - 1
        ybuf(x) = SCRH                               '' nothing drawn yet: fill to the bottom
    Next

    '' Sky first, over the whole frame. Cheaper than leaving the sky to the column loop, and
    '' it means a column that hits nothing needs no special case.
If VIDEO Then
        For i = 0 To SCRW * SCRH * 3 - 1 Step 3
            fbuf(i) = HORR : fbuf(i + 1) = HORG : fbuf(i + 2) = HORB
        Next
    Else
        Line (0, 0)-(SCRW - 1, SCRH - 1), RGB(HORR, HORG, HORB), BF
    End If

    For x = 0 To SCRW - 1
        camc = (x / (SCRW / 2.0)) - 1.0
        rx = dirx + planex * camc
        ry = diry + planey * camc
        For k = 0 To znum - 1
            z = ztab(k)
            '' The map wraps, so the camera can circle for ever without an edge. The four
            '' corners and the two weights are worked out here rather than by calling HeightAt,
            '' because the span colour below needs exactly the same four cells: a call would
            '' hide the arithmetic and then make us do all of it a second time.
            wx = camx + rx * z + MAPSZ * 4.0
            wy = camy + ry * z + MAPSZ * 4.0
            mx0 = Int(wx) And MAPMASK : mx1 = (mx0 + 1) And MAPMASK
            my0 = Int(wy) And MAPMASK : my1 = (my0 + 1) And MAPMASK
            tx  = wx - Int(wx)        : ty  = wy - Int(wy)
            i00 = my0 * MAPSZ + mx0   : i10 = my0 * MAPSZ + mx1
            i01 = my1 * MAPSZ + mx0   : i11 = my1 * MAPSZ + mx1
            hgt = (hmap(i00) * (1.0 - tx) + hmap(i10) * tx) * (1.0 - ty) _
                + (hmap(i01) * (1.0 - tx) + hmap(i11) * tx) * ty

            '' Project. Dividing by z is the whole of perspective: the same altitude
            '' difference covers fewer screen rows the further away it is.
            ytop = HORIZON + Int((camh - hgt) * VSCALE / z)

            '' THE OCCLUSION TEST, and the reason this is fast. We are walking outwards,
            '' so anything we have already drawn in this column is NEARER than this
            '' sample. If this sample's top is not above the filled line, every pixel it
            '' could occupy is already covered by something in front of it - and so is
            '' every sample further out that is no taller. Nothing to draw, nothing to
            '' test pixel by pixel: just move on.
            If ytop < ybuf(x) Then
                ybot = ybuf(x) - 1
                If ytop < 0 Then ytop = 0
                '' One vertical span, drawn once. Measured on this machine: a full-height
                '' Line is about thirteen times cheaper than the PSet calls that would
                '' cover the same pixels, which is why the renderer thinks in spans.
                '' The colour is interpolated the same way as the height, but ONLY here -
                '' inside the test, so it is paid for spans that are actually drawn and not for
                '' the samples the occlusion test throws away. Without it the surface is smooth
                '' and the paint on it is still a grid of squares, which reads as cubes just as
                '' loudly as the terraces did.
                cr = (cmR(i00) * (1.0 - tx) + cmR(i10) * tx) * (1.0 - ty) _
                   + (cmR(i01) * (1.0 - tx) + cmR(i11) * tx) * ty
                cg = (cmG(i00) * (1.0 - tx) + cmG(i10) * tx) * (1.0 - ty) _
                   + (cmG(i01) * (1.0 - tx) + cmG(i11) * tx) * ty
                cb = (cmB(i00) * (1.0 - tx) + cmB(i10) * tx) * (1.0 - ty) _
                   + (cmB(i01) * (1.0 - tx) + cmB(i11) * tx) * ty
                si = Int(z * FOGRES)
                PaintSpan(x, ytop, ybot, cr, cg, cb, fogw(si))
                ybuf(x) = ytop
                '' The column is full to the top: no sample further out can ever be seen
                '' through it. This early exit is what makes the cost sublinear in the
                '' view distance instead of proportional to it.
                If ytop <= 0 Then Exit For
            End If

        Next
    Next
End Sub

'' -------------------------------------------------------------------------------------
''  MAIN LOOP, TIMING AND THE ON-SCREEN COUNTER
'' -------------------------------------------------------------------------------------
Dim As Double ft(STATN - 1)          '' circular buffer of the last STATN frame times, ms
Dim As Double srt(STATN - 1)         '' scratch for the median
Dim As Integer fi = 0, fcount = 0
Dim As Double t0, t1, ms, med, worst, fps
Dim As Integer f, i, j
Dim As Double a, head, ghgt, camx, camy, camh
Dim As Double allft(FRAMES - 1)      '' every frame, for the report at the end

#if __SB_WASM__
    ScreenRes SCRW, SCRH, 32
#else
    Dim As Integer fh
    If VIDEO Then
        '' No ScreenRes: the offline mode never draws on a screen, so it does not need one.
        fh = FreeFile
        Open "frames.raw" For Binary Access Write As #fh
    Else
        ScreenRes SCRW, SCRH, 32
    End If
#endif
BuildWorld()
CastShadows()
PaintWorld()
BuildFog()
BuildSteps()

For f = 0 To FRAMES - 1
    t0 = Timer

    '' A closed circular path, the camera always facing along it. Closed because the run
    '' has to be repeatable: the same frame number always sees the same view, so two
    '' engines can be compared frame by frame and not just on an average.
    '' THE CAMERA PATH, and this is the third one. The first was a plain circle in 20 seconds.
    '' The second put a circle on a circle - three fast laps carried round by a slow one - to stop
    '' a minute of film repeating itself. It did stop repeating, and it was unwatchable: the
    '' interesting number is not the SHAPE of the path but the TURN RATE it implies, and three
    '' laps in a minute is 18 degrees a second of continuous yaw. Nothing in the picture is wrong;
    '' it is simply exhausting to look at. ⭐ A camera is judged in degrees per second and cells
    '' per second, not in how clever its curve is.
    ''
    '' So: one circle, one lap, sixty seconds. 6 degrees a second of yaw and a constant speed -
    '' 0.35 cells a frame, about ten a second, so the view opens rather than sweeps. It closes on
    '' its own first frame exactly, and a large radius means the ground under it is never the same
    '' ground twice, which is what the epicycle was for in the first place.
    a = f * 6.283185307179586 / FRAMES
    camx = MAPSZ / 2.0 + Cos(a) * CAMR
    camy = MAPSZ / 2.0 + Sin(a) * CAMR
    head = a + 1.5707963267948966          '' facing along the flight

    '' The eye rides the ground rather than sitting at a fixed altitude - see EYECLEAR - but it
    '' rides it on a SPRING, not glued to it. Following the terrain exactly means every hillock
    '' the camera passes over becomes a vertical movement of the entire picture, and a viewer
    '' reads that as the world moving rather than themselves. EYELAG = 0.06 means the eye closes
    '' 6% of the gap each frame - a time constant of about half a second, long enough to ignore
    '' single hills and short enough that it does not sail over a ridge and leave the ground.
    '' ⛔ And it must not be allowed to lag INTO the ground on a steep climb, hence the floor:
    '' a smooth ride is not worth flying through a mountain.
    ghgt = HeightAt(camx, camy)
    If f = 0 Then camh = ghgt + EYECLEAR
    camh += (ghgt + EYECLEAR - camh) * EYELAG
    If camh < ghgt + EYECLEAR * 0.5 Then camh = ghgt + EYECLEAR * 0.5

#if __SB_WASM__
    ScreenLock
    RenderFrame(camx, camy, head, camh)
    If fcount > 0 Then
        Locate 1, 1
        Print Using "fps ###.# | med ####.# ms | worst ####.# ms"; fps; med; worst
    End If
    ScreenUnlock
#else
    If VIDEO Then
    RenderFrame(camx, camy, head, camh)
    Put #fh, , fbuf()                       '' one frame, rgb24, straight out
    If (f Mod 60) = 0 Then Print "frame "; f; " / "; FRAMES
Else
    ScreenLock
    RenderFrame(camx, camy, head, camh)   '' facing along the flight
    '' The counter is drawn AFTER the terrain, and that is not a style choice: PRINT
    '' inside a graphics mode paints its own background, so anything drawn under it is
    '' gone. Drawn first, it would be overwritten by the landscape instead.
    If fcount > 0 Then
        Locate 1, 1
        Print Using "fps ###.# | med ####.# ms | worst ####.# ms"; fps; med; worst
    End If
    ScreenUnlock
    End If
#endif

    t1 = Timer
    ms = (t1 - t0) * 1000.0
    allft(f) = ms
    ft(fi) = ms : fi = (fi + 1) Mod STATN
    If fcount < STATN Then fcount += 1

    '' The window statistics are refreshed every 15 frames rather than every frame. The
    '' sort below is O(n^2) on 120 samples; doing it every frame would put the counter's
    '' own cost into the number the counter reports.
    If (f Mod 15) = 0 Then
        For i = 0 To fcount - 1 : srt(i) = ft(i) : Next
        SortAsc(srt(), fcount)
        med = srt(fcount \ 2)
        worst = srt(fcount - 1)
        If ms > 0 Then fps = 1000.0 / med
    End If
Next

'' -------------------------------------------------------------------------------------
''  THE REPORT. Printed to stdout so it survives a headless run, where there is no window
''  to read the on-screen counter in. The first frame is EXCLUDED from the percentiles:
''  it pays for the first touch of the two map arrays and is not a frame time.
'' -------------------------------------------------------------------------------------
Dim As Double rep(FRAMES - 2)
For i = 1 To FRAMES - 1 : rep(i - 1) = allft(i) : Next
SortAsc(rep(), FRAMES - 1)
Dim As Integer n = FRAMES - 1
Print
Print "frames measured : "; n
Print Using "median          : ####.### ms"; rep(n \ 2)
Print Using "p99             : ####.### ms"; rep(Int(n * 0.99))
Print Using "worst           : ####.### ms"; rep(n - 1)
Print Using "p99 / median    : ##.###";    rep(Int(n * 0.99)) / rep(n \ 2)
#if not __SB_WASM__
    If VIDEO Then
        Close #fh
        Print
        Print "wrote frames.raw -- "; FRAMES; " frames, rgb24, "; SCRW; "x"; SCRH
        Print "ffmpeg -f rawvideo -pix_fmt rgb24 -s "; SCRW; "x"; SCRH; " -r 30 -i frames.raw out.mp4"
    End If
#endif
