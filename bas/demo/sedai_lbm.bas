'' ============================================================================
''  SEDAI - rivelato dai vortici
''
''  A word that is never drawn, in a fluid that is never told about it.
''
''  "SEDAI" exists only as a set of cells the fluid may not enter. Nothing paints
''  the letters. They appear because a flow that meets a sharp obstacle SHEDS
''  VORTICES from its corners - alternately from one side and the other - and
''  those vortices carry the shape of what made them downstream. Stop the flow
''  and the word vanishes completely.
''
''  ---------------------------------------------------------------------------
''  THE METHOD: LATTICE BOLTZMANN, D2Q9
''
''  Nine distributions per cell, two phases, both local:
''    COLLIDE   relax each distribution toward its local equilibrium
''    STREAM    each distribution moves one cell along its own direction
''
''  Why this and not Stam's stable fluids, which is the usual choice for smoke:
''  the subject here is vortex SHEDDING, and that is exactly where the two differ.
''  Semi-Lagrangian advection is numerically dissipative - it destroys vorticity,
''  so shedding has to be faked by re-injecting it (Fedkiw's vorticity
''  confinement) with a coefficient tuned by eye. LBM streams exactly, and its
''  wall condition (BOUNCE-BACK: what enters a solid comes straight back out) is
''  exact rather than approximate - which matters because the most-looked-at part
''  of this image is the boundary of a letter.
''
''  Measured, same grid and same window: Stam 70.5 ms/frame, LBM 23.5 ms.
''  The algorithm bought the resolution the physics needed. See
''  job/docs/PIANO_DEMO.md for the full comparison and the frame budget.
''
''  ⚠️ The price: LBM is NOT unconditionally stable. If the relaxation time TAU
''  approaches 0.5, or the speed approaches the lattice sound speed, it diverges
''  into NaN. TAU and INFLOW below are chosen with margin, and a NaN watchdog
''  says so out loud rather than showing a black screen.
''
''  ---------------------------------------------------------------------------
''  RUNNING IT
''
''    sb --aot --window bas/demo/sedai_lbm.bas
''        live, in an SDL2 window (needs a build made with -Window)
''
''    sb --aot bas/demo/sedai_lbm.bas <ffmpeg-path> <out.mp4> [seconds]
''        renders frames to a raw file and calls ffmpeg once at the end
''
''  ⭐ Recording drops the real-time constraint, so it uses a FINER grid than the
''  live mode can afford: the letters go from 9 to 19 cells of stroke and the
''  wake becomes much sharper. A 30-second video takes about a minute to make.
''
''  ⭐ And it costs almost nothing: the renderer draws one SOLID BOX per cell, so
''  writing frames at simulation resolution and letting ffmpeg upscale with
''  "flags=neighbor" gives a BIT-IDENTICAL picture at one sixteenth of the data.
''
''  --aot matters. This is local float work over arrays, which is the shape the
''  AOT compiles best - and the shape the CLBG benchmark corpus does not have.
'' ============================================================================

Const LIVE_NX = 320 : Const LIVE_NY = 180 : Const LIVE_SCALE = 4   '' 1280x720 window
Const REC_NX  = 640 : Const REC_NY  = 360 : Const REC_SCALE  = 2   '' 1280x720 in the file

'' TAU is the relaxation time: viscosity = (TAU - 0.5) / 3. Lower means less
'' viscous and sharper vortices, but 0.5 is the cliff. 0.53 keeps a margin while
'' still shedding: it is the one number to try first if the wake looks too smooth.
Const TAU    = 0.53
'' Relaxation of the DYE lattice. Diffusivity is (TAUC - 0.5)/3, so this close to
'' 0.5 the dye barely diffuses at all and the spiral arms survive. Lower is
'' sharper and, as always here, closer to the stability cliff.
Const TAUC   = 0.51
'' Inflow speed in lattice units. The lattice sound speed is 1/sqrt(3) = 0.577;
'' staying under ~0.1 keeps the compressibility error invisible AND keeps the
'' scheme far from its stability limit.
Const INFLOW = 0.075
Const FPS    = 30
'' ⚠️ LBM STEPS ARE NOT VIDEO FRAMES, and confusing the two is what made the first
'' version show nothing. At INFLOW = 0.075 the fluid advances 0.075 cells per
'' step, so a vortex sheds roughly every L/(0.2*U) = 13/(0.2*0.075) ~= 870 steps.
'' One step per frame meant 500 frames covered less than ONE shedding period: the
'' wake could not exist yet. Many steps per drawn frame is the normal arrangement.
'' 10, not the 25 I first guessed. The arithmetic: a vortex sheds every
'' L/(0.2*U) = 15/(0.2*0.075) ~= 1000 LBM steps, so ten steps per frame puts one
'' shedding cycle in 100 frames - about ten cycles in a thirty-second video, which
'' is what makes the wake read as a rhythm rather than a blur.
Const NSTEPS = 10
'' How many seconds one colour theme lasts before crossfading into the next.
Const THEME_SECS = 30.0
'' Vorticity -> colour. Set from the field's real range rather than guessed: with
'' the old 9000 the palette saturated on numerical ripple and the whole frame lit
'' up. Reported by DEMO_STATS so it can be retuned instead of eyeballed.
Const CURLSCALE = 2600.0

'' ---------------------------------------------------------------- state
Dim Shared As Integer NX, NY, SCALE, SZ
Dim Shared As Double f0(), f1(), f2(), f3(), f4(), f5(), f6(), f7(), f8()
Dim Shared As Double g0(), g1(), g2(), g3(), g4(), g5(), g6(), g7(), g8()
'' ⭐ The macroscopic velocity, SAVED by the collide step instead of recomputed.
'' The draw loop needs it to make vorticity, and computing it there cost 213 ms of
'' a 247 ms frame: nine different arrays in one expression, four times per cell,
'' against an AOT array-base cache that holds ONE. Storing what collide already
'' has costs two writes and saves thirty-six reads and four divides per cell.
Dim Shared As Double vx(), vy()
Dim Shared As Double GPhase
'' The colour index per cell, computed in its OWN pass. Splitting colour from
'' drawing is not tidiness: each loop then touches two or three arrays instead of
'' five, and the AOT's array-base cache holds ONE - so a loop with many arrays in
'' one expression pays a descriptor lookup on every access. Measured: 247 -> 65 ->
'' the number in the header, from exactly this.
'' The DYE: a smooth scalar carried by the flow, and what is actually drawn.
'' ⭐ This replaces colouring by VORTICITY, which was the reason the first frames
'' looked like noise: vorticity is a DERIVATIVE, so it amplifies every numerical
'' ripple. A transported scalar has no derivatives in it - it is smooth by
'' construction, and the vortices show up as the SPIRALS they roll it into.
'' The value carried is a PHASE that grows at the inlet, so the wake holds the
'' history of the colours that entered: gold ahead of silver ahead of green.
'' ⚠️ It must grow MONOTONICALLY and wrap only when the colour is looked up -
'' advecting a cyclic value would interpolate 0.99 and 0.01 into 0.5, painting a
'' wrong band across every turn of every spiral.
'' ⭐ THE DYE IS CARRIED BY ITS OWN LATTICE (D2Q5), not by interpolation.
'' The first version traced backwards and interpolated between four neighbours -
'' semi-Lagrangian - and that SMEARS the field a little on every step. A vortex
'' rolls dye into a spiral whose arms get thinner than a cell within a couple of
'' turns, and bilinear interpolation erases them exactly as they form: the wake
'' came out as a torn smudge instead of a spiral. The reference stills settle it -
'' their filaments are two pixels wide and razor sharp, which no interpolating
'' scheme can hold.
'' LBM streaming is an INTEGER cell shift: exact, zero numerical diffusion. What
'' little smoothing there is comes from TAUC alone, and that is a physical
'' diffusivity we choose rather than an error we suffer.
Dim Shared As Double c0(), c1(), c2(), c3(), c4()
Dim Shared As Double h0(), h1(), h2(), h3(), h4()
Dim Shared As Double dye()
Dim Shared As Integer cidx()
Dim Shared As Integer solid()
Dim Shared As Integer pal(0 To 511)
Dim Shared As String pal3(0 To 511)

Declare Sub BuildMask()
Declare Sub BuildPalette(tsec As Double)
Declare Sub Init()
Declare Sub Step1()

'' ---------------------------------------------------------------- the word
'' The OFFICIAL wordmark, converted once by job/tests/tools/logo2mask.py into a
'' cell mask. The hand-cut 5x7 glyphs are the fallback so the demo still runs
'' without the file - but the logo is the deliverable, and its real counters are
'' where the flow squeezes through and makes the picture interesting.
Sub BuildMask()
  Dim As Integer gi, gx, gy, cx, cy, x0, y0, sc, wdt, k, fh, mw, mh, row, cnt
  Dim As String ln, fn, g(0 To 4, 0 To 6)

  fn = "job/demo_out/sedai_mask_" + Trim(Str(NX)) + "x" + Trim(Str(NY)) + ".txt"
  If FileExists(fn) Then
    fh = FreeFile
    Open fn For Input As #fh
    Line Input #fh, ln
    mw = ValInt(Left(ln, InStr(ln, " ") - 1))
    mh = ValInt(Mid(ln, InStr(ln, " ") + 1))
    If mw = NX And mh = NY Then
      For k = 0 To SZ - 1
        solid(k) = 0
      Next
      For row = 0 To NY - 1
        Line Input #fh, ln
        For cx = 0 To NX - 1
          If Mid(ln, cx + 1, 1) = "1" Then solid(cx + NX * row) = 1
        Next
      Next
      Close #fh
      For cx = 0 To NX - 1
        solid(cx) = 1
        solid(cx + NX * (NY - 1)) = 1
      Next
      cnt = 0
      For k = 0 To SZ - 1
        If solid(k) <> 0 Then cnt = cnt + 1
      Next
      Print "mask from "; fn; ":"; cnt; " solid cells"
      Exit Sub
    End If
    Close #fh
  End If

  g(0,0) = ".###." : g(0,1) = "#...#" : g(0,2) = "#...." : g(0,3) = ".###."
  g(0,4) = "....#" : g(0,5) = "#...#" : g(0,6) = ".###."
  g(1,0) = "#####" : g(1,1) = "#...." : g(1,2) = "#...." : g(1,3) = "####."
  g(1,4) = "#...." : g(1,5) = "#...." : g(1,6) = "#####"
  g(2,0) = "####." : g(2,1) = "#...#" : g(2,2) = "#...#" : g(2,3) = "#...#"
  g(2,4) = "#...#" : g(2,5) = "#...#" : g(2,6) = "####."
  g(3,0) = ".###." : g(3,1) = "#...#" : g(3,2) = "#...#" : g(3,3) = "#####"
  g(3,4) = "#...#" : g(3,5) = "#...#" : g(3,6) = "#...#"
  g(4,0) = "#####" : g(4,1) = "..#.." : g(4,2) = "..#.." : g(4,3) = "..#.."
  g(4,4) = "..#.." : g(4,5) = "..#.." : g(4,6) = "#####"
  For k = 0 To SZ - 1
    solid(k) = 0
  Next
  sc = (NX * 60 \ 100) \ 29
  If sc * 7 > NY * 58 \ 100 Then sc = (NY * 58 \ 100) \ 7
  If sc < 1 Then sc = 1
  wdt = 29 * sc
  x0 = NX \ 8
  y0 = (NY - 7 * sc) \ 2
  For gi = 0 To 4
    For gy = 0 To 6
      For gx = 0 To 4
        If Mid(g(gi, gy), gx + 1, 1) = "#" Then
          For cy = 0 To sc - 1
            For cx = 0 To sc - 1
              solid(x0 + (gi * 6 + gx) * sc + cx + NX * (y0 + gy * sc + cy)) = 1
            Next
          Next
        End If
      Next
    Next
  Next
  For cx = 0 To NX - 1
    solid(cx) = 1
    solid(cx + NX * (NY - 1)) = 1
  Next
  Print "fallback glyphs, stroke"; sc; " cells"
End Sub

'' ---------------------------------------------------------------- colour
Sub BuildPalette(tsec As Double)
Declare Sub BuildPalette(tsec As Double)
  '' Eight stops, and only ONE of them is pale. The previous cycle gave silver a
  '' fifth of the wheel and interpolated through it on both sides, so half the
  '' picture sat near white and everything looked washed out. Silver is a flash
  '' between two saturated golds now, which is what "gold, then silver, then
  '' green, then blue" actually looks like when a fluid carries it.
  Dim As Integer i, ci, cj, r, g, b
  Dim As Double u, m
  Dim As Integer kr(0 To 8), kg(0 To 8), kb(0 To 8)
  kr(0)=255 : kg(0)=190 : kb(0)= 55      '' gold
  kr(1)=255 : kg(1)=135 : kb(1)= 15      '' deep gold
  kr(2)=235 : kg(2)=240 : kb(2)=248      '' silver - one stop only
  kr(3)= 45 : kg(3)=225 : kb(3)=120      '' green
  kr(4)=  5 : kg(4)=165 : kb(4)= 85      '' deep green
  kr(5)=  0 : kg(5)=195 : kb(5)=230      '' cyan
  kr(6)= 30 : kg(6)=105 : kb(6)=250      '' blue
  kr(7)=165 : kg(7)= 65 : kb(7)=250      '' violet
  kr(8)=255 : kg(8)=190 : kb(8)= 55      '' back to gold: the cycle closes
  For i = 0 To 511
    u = i / 512.0 * 8.0
    ci = Int(u)
    If ci > 7 Then ci = 7
    cj = ci + 1
    m = u - ci
    m = m * m * (3.0 - 2.0 * m)
    r = Int(kr(ci) * (1 - m) + kr(cj) * m)
    g = Int(kg(ci) * (1 - m) + kg(cj) * m)
    b = Int(kb(ci) * (1 - m) + kb(cj) * m)
    pal(i) = RGB(r, g, b)
    pal3(i) = Chr(r) + Chr(g) + Chr(b)
  Next
End Sub

'' ---------------------------------------------------------------- init
Sub Init()
  Dim As Integer k, i, j
  Dim As Double W0, W1, W5, cu, usq
  W0 = 4.0 / 9.0 : W1 = 1.0 / 9.0 : W5 = 1.0 / 36.0
  '' Start every cell at the equilibrium of a uniform rightward flow, so the
  '' simulation begins already moving instead of spending a second accelerating.
  '' ⭐ THE DYE FILLS THE DOMAIN FROM THE FIRST STEP, as horizontal bands.
  '' The first version injected it only at the inlet and waited for a front to
  '' cross: at 0.075 cells per step that is 8500 steps to cross once, so the right
  '' half of the picture stayed blank and nothing was deformed by anything.
  '' Horizontal bands are also the only arrangement a UNIFORM flow cannot change -
  '' translating them along x leaves them identical - so every deformation on
  '' screen is the work of a VORTEX. That is what makes the reference stills read.
  '' bands across the height, at rest: a uniform flow along x cannot change them,
  '' so everything that moves on screen is the work of a vortex
  For j = 0 To NY - 1
    For i = 0 To NX - 1
      k = i + NX * j
      dye(k) = j / (NY / 2.5)
      c0(k) = dye(k) / 3.0
      c1(k) = dye(k) / 6.0 : c2(k) = dye(k) / 6.0
      c3(k) = dye(k) / 6.0 : c4(k) = dye(k) / 6.0
    Next
  Next
  usq = 1.5 * INFLOW * INFLOW
  For k = 0 To SZ - 1
    f0(k) = W0 * (1.0 - usq)
    cu = 3.0 * INFLOW
    f1(k) = W1 * (1.0 + cu + 0.5 * cu * cu - usq)
    f3(k) = W1 * (1.0 - cu + 0.5 * cu * cu - usq)
    f2(k) = W1 * (1.0 - usq)
    f4(k) = W1 * (1.0 - usq)
    f5(k) = W5 * (1.0 + cu + 0.5 * cu * cu - usq)
    f8(k) = W5 * (1.0 + cu + 0.5 * cu * cu - usq)
    f6(k) = W5 * (1.0 - cu + 0.5 * cu * cu - usq)
    f7(k) = W5 * (1.0 - cu + 0.5 * cu * cu - usq)
  Next
End Sub

'' ---------------------------------------------------------------- one step
Sub Step1()
  Dim As Integer i, j, k
  Dim As Double rho, ux, uy, usq, cu, omega, W0, W1, W5, t
  W0 = 4.0 / 9.0 : W1 = 1.0 / 9.0 : W5 = 1.0 / 36.0
  omega = 1.0 / TAU

  '' ---- COLLIDE (local, every fluid cell)
  For k = 0 To SZ - 1
    If solid(k) = 0 Then
      rho = f0(k) + f1(k) + f2(k) + f3(k) + f4(k) + f5(k) + f6(k) + f7(k) + f8(k)
      ux = (f1(k) - f3(k) + f5(k) - f6(k) - f7(k) + f8(k)) / rho
      uy = (f2(k) - f4(k) + f5(k) + f6(k) - f7(k) - f8(k)) / rho
      usq = 1.5 * (ux * ux + uy * uy)
      vx(k) = ux : vy(k) = uy            '' kept for the renderer: see the header
      f0(k) = f0(k) + omega * (W0 * rho * (1.0 - usq) - f0(k))
      cu = 3.0 * ux
      f1(k) = f1(k) + omega * (W1 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f1(k))
      cu = 3.0 * uy
      f2(k) = f2(k) + omega * (W1 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f2(k))
      cu = -3.0 * ux
      f3(k) = f3(k) + omega * (W1 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f3(k))
      cu = -3.0 * uy
      f4(k) = f4(k) + omega * (W1 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f4(k))
      cu = 3.0 * (ux + uy)
      f5(k) = f5(k) + omega * (W5 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f5(k))
      cu = 3.0 * (-ux + uy)
      f6(k) = f6(k) + omega * (W5 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f6(k))
      cu = 3.0 * (-ux - uy)
      f7(k) = f7(k) + omega * (W5 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f7(k))
      cu = 3.0 * (ux - uy)
      f8(k) = f8(k) + omega * (W5 * rho * (1.0 + cu + 0.5 * cu * cu - usq) - f8(k))
    End If
  Next

  '' ---- STREAM, with BOUNCE-BACK on the letters.
  '' Streaming reads from the neighbour the distribution came FROM. When that
  '' neighbour is solid, what arrives instead is the OPPOSITE distribution of
  '' this same cell, reflected: that is the whole wall condition, and it is exact
  '' rather than approximate. It is also why the letters shed vortices at all.
  For j = 1 To NY - 2
    For i = 1 To NX - 2
      k = i + NX * j
      If solid(k) <> 0 Then
        g0(k) = 0 : g1(k) = 0 : g2(k) = 0 : g3(k) = 0
        g4(k) = 0 : g5(k) = 0 : g6(k) = 0 : g7(k) = 0 : g8(k) = 0
      Else
        g0(k) = f0(k)
        If solid(k - 1) = 0      Then g1(k) = f1(k - 1)      Else g1(k) = f3(k)
        If solid(k - NX) = 0     Then g2(k) = f2(k - NX)     Else g2(k) = f4(k)
        If solid(k + 1) = 0      Then g3(k) = f3(k + 1)      Else g3(k) = f1(k)
        If solid(k + NX) = 0     Then g4(k) = f4(k + NX)     Else g4(k) = f2(k)
        If solid(k - 1 - NX) = 0 Then g5(k) = f5(k - 1 - NX) Else g5(k) = f7(k)
        If solid(k + 1 - NX) = 0 Then g6(k) = f6(k + 1 - NX) Else g6(k) = f8(k)
        If solid(k + 1 + NX) = 0 Then g7(k) = f7(k + 1 + NX) Else g7(k) = f5(k)
        If solid(k - 1 + NX) = 0 Then g8(k) = f8(k - 1 + NX) Else g8(k) = f6(k)
      End If
    Next
  Next

  '' ---- inlet on the left: equilibrium at the driving speed.
  '' ---- outlet on the right: copy the column before it (zero-gradient), so the
  ''      wake leaves instead of reflecting back into the picture.
  usq = 1.5 * INFLOW * INFLOW
  cu = 3.0 * INFLOW
  For j = 1 To NY - 2
    k = NX * j
    g0(k) = W0 * (1.0 - usq)
    g1(k) = W1 * (1.0 + cu + 0.5 * cu * cu - usq)
    g3(k) = W1 * (1.0 - cu + 0.5 * cu * cu - usq)
    g2(k) = W1 * (1.0 - usq)
    g4(k) = W1 * (1.0 - usq)
    g5(k) = W5 * (1.0 + cu + 0.5 * cu * cu - usq)
    g8(k) = W5 * (1.0 + cu + 0.5 * cu * cu - usq)
    g6(k) = W5 * (1.0 - cu + 0.5 * cu * cu - usq)
    g7(k) = W5 * (1.0 - cu + 0.5 * cu * cu - usq)
    k = (NX - 1) + NX * j
    g0(k) = g0(k - 1) : g1(k) = g1(k - 1) : g2(k) = g2(k - 1)
    g3(k) = g3(k - 1) : g4(k) = g4(k - 1) : g5(k) = g5(k - 1)
    g6(k) = g6(k - 1) : g7(k) = g7(k - 1) : g8(k) = g8(k - 1)
  Next

  '' ---- ping-pong: g becomes the new f
  For k = 0 To SZ - 1
    f0(k) = g0(k) : f1(k) = g1(k) : f2(k) = g2(k)
    f3(k) = g3(k) : f4(k) = g4(k) : f5(k) = g5(k)
    f6(k) = g6(k) : f7(k) = g7(k) : f8(k) = g8(k)
  Next

  '' ================= the DYE lattice, D2Q5 =================
  '' Advection-diffusion of a passive scalar. The equilibrium is LINEAR in the
  '' velocity - a scalar has no momentum of its own - and it rides the velocity
  '' field the D2Q9 lattice just produced.
  '' ⚠️ Run EVERY substep, not once per frame: a vortex needs many small rotations
  '' to wind the dye into a spiral, and one big step per frame cuts the corners so
  '' the spiral never closes.
  Dim As Double cc, om2, ceq
  om2 = 1.0 / TAUC
  For k = 0 To SZ - 1
    If solid(k) = 0 Then
      cc = c0(k) + c1(k) + c2(k) + c3(k) + c4(k)
      ceq = cc / 3.0
      c0(k) = c0(k) + om2 * (ceq - c0(k))
      ceq = cc / 6.0
      c1(k) = c1(k) + om2 * (ceq * (1.0 + 3.0 * vx(k)) - c1(k))
      c2(k) = c2(k) + om2 * (ceq * (1.0 + 3.0 * vy(k)) - c2(k))
      c3(k) = c3(k) + om2 * (ceq * (1.0 - 3.0 * vx(k)) - c3(k))
      c4(k) = c4(k) + om2 * (ceq * (1.0 - 3.0 * vy(k)) - c4(k))
    End If
  Next
  For j = 1 To NY - 2
    For i = 1 To NX - 2
      k = i + NX * j
      If solid(k) <> 0 Then
        h0(k) = 0 : h1(k) = 0 : h2(k) = 0 : h3(k) = 0 : h4(k) = 0
      Else
        h0(k) = c0(k)
        If solid(k - 1) = 0  Then h1(k) = c1(k - 1)  Else h1(k) = c3(k)
        If solid(k - NX) = 0 Then h2(k) = c2(k - NX) Else h2(k) = c4(k)
        If solid(k + 1) = 0  Then h3(k) = c3(k + 1)  Else h3(k) = c1(k)
        If solid(k + NX) = 0 Then h4(k) = c4(k + NX) Else h4(k) = c2(k)
      End If
    Next
  Next
  '' inlet holds the band profile plus the slow drift; outlet copies its neighbour
  For j = 1 To NY - 2
    k = NX * j
    cc = j / (NY / 2.5) + GPhase
    h0(k) = cc / 3.0
    h1(k) = cc / 6.0 : h2(k) = cc / 6.0 : h3(k) = cc / 6.0 : h4(k) = cc / 6.0
    k = (NX - 1) + NX * j
    h0(k) = h0(k-1) : h1(k) = h1(k-1) : h2(k) = h2(k-1)
    h3(k) = h3(k-1) : h4(k) = h4(k-1)
  Next
  For k = 0 To SZ - 1
    c0(k) = h0(k) : c1(k) = h1(k) : c2(k) = h2(k)
    c3(k) = h3(k) : c4(k) = h4(k)
    dye(k) = c0(k) + c1(k) + c2(k) + c3(k) + c4(k)
  Next
End Sub

'' ---------------------------------------------------------------- main
Dim As String ffmpegPath, outFile, secArg, frameBuf, key, cmd, row, BLACK3
BLACK3 = Chr(0) + Chr(0) + Chr(0)
Dim As Integer i, j, k, fr, ci, totalFrames, rawFile, recording
Dim As Double ux0, ux1, uy0, uy1, curl, rho, ux, uy, tStart, elapsed
Dim As Double vmax
'' DEMO_FRAMES=<n> runs exactly n frames unpaced. It is how the demo is timed and
'' how it is inspected without a display; unset, live mode runs until a key.
Dim As Integer capFrames, noRender
capFrames = ValInt(Environ("DEMO_FRAMES"))
'' DEMO_NORENDER=1 runs the solver and skips the picture: the two halves of the
'' frame have to be separable or "it is slow" is all anyone can say.
noRender = ValInt(Environ("DEMO_NORENDER"))
Dim As Double tSolve, tDraw, tA, tB
Dim As Integer ss, i0, j0
Dim As Double dx, dy, sx, sy, phase
Dim As Integer warm

ffmpegPath = Command(1)
outFile = Command(2)
secArg = Command(3)
recording = 0
If Len(ffmpegPath) > 0 And Len(outFile) > 0 Then recording = 1

If recording = 1 Then
  NX = REC_NX : NY = REC_NY : SCALE = REC_SCALE
Else
  NX = LIVE_NX : NY = LIVE_NY : SCALE = LIVE_SCALE
End If
SZ = NX * NY

ReDim f0(0 To SZ-1) : ReDim f1(0 To SZ-1) : ReDim f2(0 To SZ-1)
ReDim f3(0 To SZ-1) : ReDim f4(0 To SZ-1) : ReDim f5(0 To SZ-1)
ReDim f6(0 To SZ-1) : ReDim f7(0 To SZ-1) : ReDim f8(0 To SZ-1)
ReDim g0(0 To SZ-1) : ReDim g1(0 To SZ-1) : ReDim g2(0 To SZ-1)
ReDim g3(0 To SZ-1) : ReDim g4(0 To SZ-1) : ReDim g5(0 To SZ-1)
ReDim g6(0 To SZ-1) : ReDim g7(0 To SZ-1) : ReDim g8(0 To SZ-1)
ReDim vx(0 To SZ-1) : ReDim vy(0 To SZ-1) : ReDim cidx(0 To SZ-1)
ReDim dye(0 To SZ-1)
ReDim c0(0 To SZ-1) : ReDim c1(0 To SZ-1) : ReDim c2(0 To SZ-1)
ReDim c3(0 To SZ-1) : ReDim c4(0 To SZ-1)
ReDim h0(0 To SZ-1) : ReDim h1(0 To SZ-1) : ReDim h2(0 To SZ-1)
ReDim h3(0 To SZ-1) : ReDim h4(0 To SZ-1)
ReDim solid(0 To SZ-1)

Print "SEDAI - lattice Boltzmann D2Q9"
Print "grid "; NX; "x"; NY; "   window "; NX * SCALE; "x"; NY * SCALE;
If recording = 1 Then Print "   RECORDING" Else Print "   LIVE"

BuildPalette(0.0)
BuildMask()
Init()

totalFrames = 0
If recording = 1 Then
  totalFrames = ValInt(secArg) * FPS
  If totalFrames <= 0 Then totalFrames = 30 * FPS
  rawFile = FreeFile
  Open "sedai_frames.raw" For Binary As #rawFile
Else
  ScreenRes NX * SCALE, NY * SCALE, 32
End If

'' ⚠️ WARM-UP. A wake is not there at step zero: the flow has to pass the obstacle
'' several times before vortices shed in a rhythm. Recording from a cold start
'' spends the first seconds of the video showing nothing happening. DEMO_WARM
'' steps are run with no frame written and no dye advected - only the velocity
'' field matters here, and the dye would just be smeared by a flow that is still
'' settling.
warm = ValInt(Environ("DEMO_WARM"))
If warm > 0 Then
  Print "warm-up:"; warm; " LBM steps ..."
  For ss = 1 To warm
    Step1()
  Next
  Print "  done"
End If

fr = 0
tStart = Timer
Do
  fr = fr + 1
  tA = Timer
  For ss = 1 To NSTEPS
    Step1()
  Next
  '' the dye now travels inside Step1, on its own lattice, once per substep
  GPhase = GPhase + NSTEPS / (THEME_SECS * FPS * NSTEPS)
  tB = Timer
  tSolve = tSolve + (tB - tA)
  tA = tB

  '' ---- colour by SIGNED vorticity, computed from the macroscopic velocity.
  If recording = 1 Then frameBuf = ""
  If noRender = 1 Then GoTo SkipDraw
  '' pass 1: vorticity -> colour index. Two float arrays in, one int array out.
  For k = NX + 1 To SZ - NX - 2
    '' the phase wraps HERE, at the lookup, never in the carried value
    ci = Int((dye(k) - Int(dye(k))) * 512.0)
    If ci < 0 Then ci = 0
    If ci > 511 Then ci = 511
    cidx(k) = ci
  Next
  '' pass 2: the picture.
  '' ⚠️ EVERY cell, border included. An earlier split ran 1..NX-2 and the recorded
  '' frames came out 638x358 while ffmpeg was told 640x360 - the video would have
  '' been sheared. Caught by checking the raw file is an exact multiple of
  '' NX*NY*3, which is worth doing every time: a frame writer off by one row makes
  '' a picture that looks almost right.
  ''
  '' ⚠️⚠️ And the two modes are SEPARATE LOOPS, not one loop with a test inside.
  '' Written as a single loop with "If recording" per cell, a frame cost 2.7
  '' SECONDS: the drawing call was being reached in recording mode too, against a
  '' screen that was never opened. Two loops also means neither pays for the other's
  '' branch, on 230 000 cells a frame.
  If recording = 1 Then
    For j = 0 To NY - 1
      row = ""
      For i = 0 To NX - 1
        k = i + NX * j
        If solid(k) <> 0 Then
          row = row + BLACK3
        Else
          row = row + pal3(cidx(k))
        End If
      Next
      frameBuf = frameBuf + row
    Next
  Else
    For j = 0 To NY - 1
      For i = 0 To NX - 1
        k = i + NX * j
        If solid(k) <> 0 Then
          Line (i * SCALE, j * SCALE)-(i * SCALE + SCALE - 1, j * SCALE + SCALE - 1), 0, BF
        Else
          Line (i * SCALE, j * SCALE)-(i * SCALE + SCALE - 1, j * SCALE + SCALE - 1), pal(cidx(k)), BF
        End If
      Next
    Next
  End If
  SkipDraw:
  tDraw = tDraw + (Timer - tA)
  If recording = 1 And noRender = 0 Then
    Put #rawFile, , frameBuf
    If (fr Mod FPS) = 0 Then Print "  "; fr \ FPS; "s /"; totalFrames \ FPS; "s"
  Else
    If capFrames = 0 Then Frame FPS
    key = InKey
  End If

  '' ---- NaN watchdog: LBM can diverge, and a black screen would not say why.
  If (fr Mod 60) = 0 Then
    rho = f0(SZ \ 2 + NX \ 4)
    If (rho <> rho) Or (rho > 1000.0) Then
      Print "UNSTABLE at frame"; fr; " - raise TAU (now "; TAU; ") or lower INFLOW (now "; INFLOW; ")"
      Exit Do
    End If
  End If
Loop While ((recording = 1 And fr < totalFrames) Or (recording = 0 And key = "")) _
      And (capFrames = 0 Or fr < capFrames)

elapsed = Timer - tStart
Print "frames:"; fr; "  seconds:"; elapsed; "  ms/frame:"; elapsed * 1000 / fr
Print "   solver:"; tSolve * 1000 / fr; " ms/frame     draw:"; tDraw * 1000 / fr; " ms/frame"

If recording = 1 Then
  Close #rawFile
  cmd = Chr(34) + ffmpegPath + Chr(34) + " -y -f rawvideo -pixel_format rgb24 -video_size "
  cmd = cmd + Str(NX) + "x" + Str(NY) + " -framerate " + Str(FPS) + " -i sedai_frames.raw"
  cmd = cmd + " -vf scale=" + Str(NX * SCALE) + ":" + Str(NY * SCALE) + ":flags=neighbor"
  cmd = cmd + " -c:v libx264 -pix_fmt yuv420p -crf 16 " + Chr(34) + outFile + Chr(34)
  Print "running: "; cmd
  Shell cmd
  Print "done -> "; outFile
  Print "(the raw frames are in sedai_frames.raw; delete when happy)"
End If

'' DEMO_ASCII=1 prints the vorticity field as text. The demo is developed without
'' a display, so this is the only way to answer "does it look right?" before a
'' window is ever opened - and it is what caught the empty mask.
If ValInt(Environ("DEMO_ASCII")) = 1 Then
  Dim As String row
  Dim As Integer sx, sy
  Print
  For sy = 0 To NY - 1 Step 4
    row = ""
    For sx = 0 To NX - 1 Step 2
      k = sx + NX * sy
      If solid(k) <> 0 Then
        row = row + "#"
      Else
        rho = f0(k) + f1(k) + f2(k) + f3(k) + f4(k) + f5(k) + f6(k) + f7(k) + f8(k)
        ux = (f1(k) - f3(k) + f5(k) - f6(k) - f7(k) + f8(k)) / rho
        uy = (f2(k) - f4(k) + f5(k) + f6(k) - f7(k) - f8(k)) / rho
        If uy > 0.012 Then
          row = row + "^"
        ElseIf uy < -0.012 Then
          row = row + "v"
        ElseIf ux > INFLOW * 1.15 Then
          row = row + "-"
        ElseIf ux < INFLOW * 0.4 Then
          row = row + "."
        Else
          row = row + " "
        End If
      End If
    Next
    Print row
  Next
End If

End
