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
''  For each of the 320 screen columns we send a ray out across the map and walk it from
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
'' Measured, so the offline cost is not a surprise: a 1080p frame takes 107 ms to fill and
'' write against 54 ms to draw on screen - the byte-at-a-time array writes cost about twice
'' what Line does, which is the price of a format that cannot be got wrong. 600 frames is
'' about a minute of rendering, and ⚠️ 3.7 GB of raw file: pipe it or delete it after.
#define VIDEO 0

#if VIDEO
    Const SCRW  = 1920
    Const SCRH  = 1080
#else
    Const SCRW  = 640          '' the crossover point, measured: at this size the traversal
    Const SCRH  = 480          ''   and the span filling cost about the same (52% / 48%)
#endif
Const HORIZON   = SCRH * 2 \ 5 '' screen row of the eye line. Below centre, so more of the
                               ''   picture is ground than sky - the ground is the subject.
Const MAPSZ     = 256          '' power of two so wrapping is an AND, not a modulo. Small
Const MAPMASK   = MAPSZ - 1    ''   enough that world generation takes well under a second
                               ''   interpreted; the camera circle never reveals the repeat.
Const SEED      = 20260806     '' fixed: two runs must produce the same terrain, or the
                               ''   frame times below would not be comparable.
Const ZNEAR     = 1.0          '' first sample distance. Closer than this the projection
                               ''   divides by almost nothing and one sample fills the screen.
Const ZFAR      = 220.0        '' where the world ends and the sky begins. Found by walking
                               ''   the camera and raising it until no more detail appeared:
                               ''   past ~220 the terrain is under a pixel tall.
Const ZSTEP     = 1.012        '' the step GROWTH factor - see the note in RenderFrame.
Const VSCALE    = 0.75 * SCRH   '' vertical exaggeration, as a fraction of the screen height
                               ''   so the framing does not change with the resolution.
                               ''   Empirical at 200 rows: 100 looked like a pancake, 250
                               ''   turned every slope into a cliff; 150 = 0.75 * 200.
Const EYECLEAR     = 45.0         '' how far the eye rides ABOVE the ground under it. Not
                               ''   named CLEAR: that is a FreeBASIC keyword (the memset).
                               '' ⚠️ This replaced a fixed eye altitude, and the instrument
                               '' is what said so. With the camera pinned at 165 units the
                               '' step counter reported the SAME 144960 samples on almost
                               '' every frame - 320 columns x 453 steps, i.e. every column
                               '' walking the full distance and the y-buffer's early exit
                               '' never firing once. The renderer was correct and the
                               '' occlusion property this file spends a page explaining was
                               '' doing nothing, because from that height no ridge ever
                               '' reached the top of the screen. Three frames were worse
                               '' still: 320 samples and a blank picture, the camera having
                               '' flown straight through a peak taller than it was.
                               '' Riding the terrain fixes both: near ridges now fill their
                               '' columns, columns exit early, and the eye can never be
                               '' inside a hill.
Const CAMR      = 70.0         '' radius of the camera circle, in map cells.
Const FRAMES    = 600          '' one full circle. The run ends here so it can be measured.
Const STATN     = 120          '' frame-time window the on-screen counter reports on.

Dim Shared As UByte    hmap(MAPSZ * MAPSZ - 1)   '' altitude, 0..255
'' The surface colour is kept BOTH ways, and the reason is portability, not convenience.
'' RGB() packs its three channels into an integer whose BYTE ORDER is the compiler's business,
'' not ours - so a raw file written from packed values would come out with red and blue
'' swapped on one of the two targets and nobody would notice until the video was watched.
'' The packed form feeds Line, which wants exactly what RGB() returns; the three byte arrays
'' feed the raw file, where every byte's meaning is written down. 192 KB for the three at
'' this map size, which buys a format that cannot be got wrong.
Dim Shared As UInteger cmap(MAPSZ * MAPSZ - 1)   '' surface colour, precomputed with it
Dim Shared As UByte    cmR(MAPSZ * MAPSZ - 1)
Dim Shared As UByte    cmG(MAPSZ * MAPSZ - 1)
Dim Shared As UByte    cmB(MAPSZ * MAPSZ - 1)
#if VIDEO
    '' One frame, rgb24, exactly the bytes ffmpeg is told to expect.
    Dim Shared As UByte fbuf(SCRW * SCRH * 3 - 1)
#endif
Dim Shared As Integer  ybuf(SCRW - 1)            '' the occlusion state, one row per column

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

Sub BuildWorld()
    Dim As Integer x, y, h
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
            h = Int(n * 255.0)
            If h < 0 Then h = 0
            If h > 255 Then h = 255
            hmap(y * MAPSZ + x) = h
            '' Phase 1 keeps the colour a plain function of altitude: dark green in the
            '' valleys through to pale grey on the tops. Banding with soft transitions is
            '' a phase 2 job; doing it here would hide whether the RENDERER is right.
            cmR(y * MAPSZ + x) = 60 + h \ 3
            cmG(y * MAPSZ + x) = 90 + h \ 2
            cmB(y * MAPSZ + x) = 60 + h \ 4
            cmap(y * MAPSZ + x) = RGB(60 + h \ 3, 90 + h \ 2, 60 + h \ 4)
        Next
    Next
End Sub

'' The one routine that differs between the two modes. Everything above and below it - the
'' world, the camera, the traversal, the y-buffer - is shared, which is the point: the video
'' is of the same renderer, not of a second one that drifted.
'' In VIDEO mode there is no Line and no screen to draw on; the span goes straight into the
'' frame as bytes. That also makes the video build HEADLESS: it needs no display at all,
'' which is how an offline render actually gets run.
Sub PaintSpan(ByVal x As Integer, ByVal y0 As Integer, ByVal y1 As Integer, ByVal ci As Integer)
#if VIDEO
    Dim As Integer i, o
    For i = y0 To y1
        o = (i * SCRW + x) * 3
        fbuf(o)     = cmR(ci)
        fbuf(o + 1) = cmG(ci)
        fbuf(o + 2) = cmB(ci)
    Next
#else
    Line (x, y0)-(x, y1), cmap(ci)
#endif
End Sub

'' -------------------------------------------------------------------------------------
''  THE RENDERER
'' -------------------------------------------------------------------------------------
Sub RenderFrame(ByVal camx As Double, ByVal camy As Double, ByVal ang As Double, ByVal camh As Double)
    Dim As Integer x, i, mx, my, hgt, ytop, ybot
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
#if VIDEO
    For i = 0 To SCRW * SCRH * 3 - 1 Step 3
        fbuf(i) = 120 : fbuf(i + 1) = 160 : fbuf(i + 2) = 210
    Next
#else
    Line (0, 0)-(SCRW - 1, SCRH - 1), RGB(120, 160, 210), BF
#endif

    For x = 0 To SCRW - 1
        camc = (x / (SCRW / 2.0)) - 1.0
        rx = dirx + planex * camc
        ry = diry + planey * camc
        z = ZNEAR
        Do While z < ZFAR
            wx = camx + rx * z
            wy = camy + ry * z
            mx = Int(wx) And MAPMASK                 '' the map wraps, so the camera can
            my = Int(wy) And MAPMASK                 ''   circle for ever without an edge
            hgt = hmap(my * MAPSZ + mx)

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
                PaintSpan(x, ytop, ybot, my * MAPSZ + mx)
                ybuf(x) = ytop
                '' The column is full to the top: no sample further out can ever be seen
                '' through it. This early exit is what makes the cost sublinear in the
                '' view distance instead of proportional to it.
                If ytop <= 0 Then Exit Do
            End If

            '' STEP GROWTH. One screen column covers more world space the further we
            '' look, because the rays diverge. A constant step therefore OVERSAMPLES near
            '' the camera - many samples landing on the same pixel, all but one of them
            '' wasted - and UNDERSAMPLES at the horizon, where consecutive samples skip
            '' whole map cells and the ridges break into shimmering dotted lines that
            '' crawl as the camera moves. Growing the step geometrically keeps the sample
            '' spacing roughly constant in SCREEN space, which is where it matters.
            '' 1.012 is empirical: below ~1.005 the extra samples cost time and change
            '' nothing visible, above ~1.02 the far ridges start to break up.
            z = z * ZSTEP
        Loop
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
Dim As Double a, camx, camy, camh
Dim As Double allft(FRAMES - 1)      '' every frame, for the report at the end

#if VIDEO
    '' No ScreenRes: the video build never draws on a screen, so it does not need one.
    Dim As Integer fh = FreeFile
    Open "frames.raw" For Binary Access Write As #fh
#else
    ScreenRes SCRW, SCRH, 32
#endif
BuildWorld()

For f = 0 To FRAMES - 1
    t0 = Timer

    '' A closed circular path, the camera always facing along it. Closed because the run
    '' has to be repeatable: the same frame number always sees the same view, so two
    '' engines can be compared frame by frame and not just on an average.
    a = f * 6.283185307179586 / FRAMES
    camx = MAPSZ / 2.0 + Cos(a) * CAMR
    camy = MAPSZ / 2.0 + Sin(a) * CAMR
    '' The eye rides the ground rather than sitting at a fixed altitude - see EYECLEAR.
    camh = hmap((Int(camy) And MAPMASK) * MAPSZ + (Int(camx) And MAPMASK)) + EYECLEAR

#if VIDEO
    RenderFrame(camx, camy, a + 1.5707963267948966, camh)
    Put #fh, , fbuf()                       '' one frame, rgb24, straight out
    If (f Mod 60) = 0 Then Print "frame "; f; " / "; FRAMES
#else
    ScreenLock
    RenderFrame(camx, camy, a + 1.5707963267948966, camh)   '' facing along the circle
    '' The counter is drawn AFTER the terrain, and that is not a style choice: PRINT
    '' inside a graphics mode paints its own background, so anything drawn under it is
    '' gone. Drawn first, it would be overwritten by the landscape instead.
    If fcount > 0 Then
        Locate 1, 1
        Print Using "fps ###.# | med ####.# ms | worst ####.# ms"; fps; med; worst
    End If
    ScreenUnlock
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
#if VIDEO
    Close #fh
    Print
    Print "wrote frames.raw -- "; FRAMES; " frames, rgb24, "; SCRW; "x"; SCRH
    Print "ffmpeg -f rawvideo -pix_fmt rgb24 -s "; SCRW; "x"; SCRH; " -r 30 -i frames.raw out.mp4"
#endif
