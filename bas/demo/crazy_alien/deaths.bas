'' ================================================================================================
''  CRAZY ALIEN - procedural death effects, an exploratory prototype
'' ================================================================================================
''  One question: do computed deaths (no hand-drawn animation) hold up visually at ANY speed and
''  inside the frame budget? No game, no movement, no weapon, no score - only the deaths.
''
''  THE PRINCIPLE. Five effects share one road: the sprite's pixels become independent FRAGMENTS
''  (position, velocity, colour) and each effect is a different force field over that list. The
''  sprite is DATA and its size is read from the data, so any sprite works. The sixth (ghost) is
''  the exception: it transforms the INTACT sprite, a second code path - see GHOST below.
''
''  THE DURATION RULE. Every death takes a duration D and must read at every D, so all motion is
''  written in NORMALISED time p = age / D (0 at the hit, 1 at the end): velocities are "pixels per
''  whole death", accelerations "pixels per death squared". The trajectory is the SAME shape at any
''  D - a short D only plays it faster. Nothing has a duration of its own baked in.
''
''  Keys: 1-6 effect, + / - duration (shown), S twenty at once (worst case), Q quit. Headless runs
''  (bench, capture) are described at MAIN. Transparency is FAKED: the background is black, so
''  "alpha a" is drawn as colour * a; over a starfield it would need a read-blend-write per pixel.
'' ================================================================================================
Const SCREEN_W = 640, SCREEN_H = 480
Const DUR_MIN = 0.05, DUR_MAX = 2.0, DUR_STEP = 0.05
Const MAX_FRAGS = 12000, MAX_GHOSTS = 24, MAX_HOLES = 24, STATS_N = 120
Const K_DISINTEGRATE = 1, K_TORNADO = 2, K_FREEZE = 3, K_CHAR = 4, K_BLACKHOLE = 5, K_GHOST = 6

Type Fragment
  x As Single : y As Single : vx As Single : vy As Single
  r As Single : g As Single : b As Single          '' colour at spawn; Render derives the rest
  age As Single : dur As Single : seed As Single   '' seed: one random per fragment, fixed at spawn
  kind As Integer : hole As Integer                '' hole: index of the black hole that owns it
  ax As Single : ay As Single                      '' anchor: sprite centre, or the impact point
End Type
Type Floater                                        '' a ghost, or a black hole's point
  x As Single : y As Single : age As Single : dur As Single
End Type

Dim Shared As Fragment frags(0 To MAX_FRAGS - 1)
Dim Shared As Floater ghosts(0 To MAX_GHOSTS - 1), holes(0 To MAX_HOLES - 1)
Dim Shared As Integer nFrags, nGhosts, nHoles, sprW, sprH, frameIdx
Dim Shared As Integer sprite(0 To 31, 0 To 31)     '' 0 = transparent, else packed RGB
Dim Shared As Double frameMs(0 To STATS_N - 1)

'' THE TEST SPRITE: a 16x16 robotic crab with eyes on stalks. The palette row (letters, then their
'' colours: body, shadow, eye, pupil, metal, mouth, feet) comes first; '.' is air. Width and height
'' are read from the data, so a 24x24 sprite is a data change only.
Data "BDEPMRY", "46C85A", "1E7832", "F0F0FF", "141428", "A0A5AF", "DC323C", "FADC3C"
Data 16, 16
Data "...EE.....EE....", "..EPPE...EPPE...", "..EPPE...EPPE...", "...EE.....EE...."
Data "....D.....D.....", "...DBBBBBBBD....", "..DBBBBBBBBBD...", ".DBBBMBBBMBBBD.."
Data ".DBBBBBBBBBBBD..", "..DBBRRRRRBBD...", "...DBBBBBBBD....", "...M.MDDDM.M...."
Data "..M..M...M..M...", ".M...M...M...M..", ".....Y...Y......", "....YY...YY....."

Sub LoadSprite()
  Dim As Integer x, y, i, pal(0 To 6)
  Dim As String row, letters
  Read letters
  For i = 0 To 6 : Read row : pal(i) = Val("&H" + row) : Next   '' hex as text: portable DATA
  Read sprW, sprH
  For y = 0 To sprH - 1
    Read row
    For x = 0 To sprW - 1
      i = Instr(letters, Mid(row, x + 1, 1))
      If i > 0 Then sprite(x, y) = pal(i - 1) Else sprite(x, y) = 0
    Next
  Next
End Sub

'' Every "turn to ice / to black / to pale" below is this one line: the effects differ in FIELDS.
Function Lerp( ByVal a As Single, ByVal b As Single, ByVal t As Single ) As Single
  If t < 0 Then t = 0
  If t > 1 Then t = 1
  Return a + (b - a) * t
End Function

Function C255( ByVal r As Single, ByVal g As Single, ByVal b As Single ) As Integer
  Return RGB(Int(Lerp(0, 255, r / 255)), Int(Lerp(0, 255, g / 255)), Int(Lerp(0, 255, b / 255)))
End Function

'' SPAWN - one call per death. This dispatcher is the ONLY code the two paths share, besides the
'' age/dur clock, Lerp and the sprite table.
Sub Spawn( ByVal kind As Integer, ByVal sx As Integer, ByVal sy As Integer, ByVal dur As Single )
  Dim As Integer x, y, c, h = -1
  Dim As Single cx = sx + sprW / 2, cy = sy + sprH / 2
  If kind = K_GHOST Then
    If nGhosts >= MAX_GHOSTS Then Exit Sub
    ghosts(nGhosts).x = sx : ghosts(nGhosts).y = sy : ghosts(nGhosts).age = 0 : ghosts(nGhosts).dur = dur
    nGhosts += 1
    Exit Sub
  End If
  If kind = K_BLACKHOLE Then
    '' The attractor sits above and beside the sprite so the stretch has a direction to read; at
    '' the centre every fragment would only shrink and the spaghetti would not show.
    If nHoles >= MAX_HOLES Then Exit Sub
    h = nHoles : nHoles += 1
    holes(h).x = cx + sprW * 0.7 : holes(h).y = cy - sprH * 0.8 : holes(h).age = 0 : holes(h).dur = dur
  End If
  For y = 0 To sprH - 1
    For x = 0 To sprW - 1
      c = sprite(x, y)
      If c = 0 Or nFrags >= MAX_FRAGS Then Continue For
      With frags(nFrags)
        .x = sx + x : .y = sy + y : .vx = 0 : .vy = 0
        .r = (c Shr 16) And 255 : .g = (c Shr 8) And 255 : .b = c And 255
        .age = 0 : .dur = dur : .seed = Rnd : .kind = kind : .hole = h : .ax = cx : .ay = cy
        If kind = K_DISINTEGRATE Then
          '' Radial burst with an upward bias: a symmetric burst reads as a "pop", the bias as a
          '' "blast". 180 px/death sideways, ~5 sprite heights, found by eye; below ~80 the cloud
          '' looks stuck to the spot at short durations.
          .vx = (Rnd - 0.5) * 180 : .vy = -Rnd * 110 - 20
        ElseIf kind = K_CHAR Then
          .ax = sx : .ay = cy                      '' impact point: the left edge, a shot from the side
        End If
      End With
      nFrags += 1
    Next
  Next
End Sub

'' UPDATE. dt is wall seconds, dp = dt / dur the normalised step every field is written in: VELOCITY
'' fields (x += v*dp) where the shape must be duration-invariant, ACCELERATION fields (v += a*dp)
'' where inertia is the point (gravity, pull).
Sub UpdateFragments( ByVal dt As Single )
  Dim As Integer i = 0
  Dim As Single p, dp, dx, dy, dist, q
  While i < nFrags
    With frags(i)
      dp = dt / .dur : .age += dt : p = .age / .dur
      Select Case .kind
        Case K_DISINTEGRATE
          '' Gravity 260 px/death^2: at D = 1 s the cloud falls ~130 px, at D = 0.15 s the same
          '' arc in 0.15 s. Halve it and the cloud hangs; double it and it is a downward smear.
          .vy += 260 * dp
          .x += .vx * dp : .y += .vy * dp
        Case K_TORNADO
          '' Rotation about an axis that itself RISES (ay -= 90*dp): 10..16 rad per death (the
          '' seed spreads the angular speed, so the cloud SMEARS into a spiral instead of orbiting
          '' as one block, which reads as "it flew away"), radius growing exponentially (dx*1.0).
          '' Six radians reads as a wobble; radius growth 1.6 flung the cloud off the sprite.
          .ay -= 90 * dp : dx = .x - .ax : dy = .y - .ay
          q = 10 + 6 * .seed
          .x += (-dy * q + dx * 1.0) * dp : .y += (dx * q - 40) * dp
        Case K_FREEZE
          '' Phase 1 (p < 0.35): NOTHING moves - the freeze reads only because the sprite holds
          '' still while its colour drains (done in Render). Phase 2: gravity (220) and no sideways
          '' velocity, so the pieces DRIP instead of bursting; the seed staggers the release so the
          '' sprite comes apart over ~15% of the death. At 90 the drip was ~20 px and unreadable.
          If p > 0.35 + .seed * 0.15 Then .vy += 220 * dp : .y += .vy * dp
        Case K_CHAR
          '' The front is drawn in Render; here only the crumble. A fragment is released once the
          '' front has passed it AND a seed-staggered delay has elapsed; then a lateral wind (140)
          '' carries it while a light gravity (40) settles it - ash, not rubble. Wind alone is a
          '' horizontal streak; gravity alone is disintegration.
          dx = .x - .ax : dy = .y - .ay
          If p > 0.40 + .seed * 0.30 And dx * dx + dy * dy < (p * 2.2 * sprW) ^ 2 Then
            .vx += 140 * dp : .vy += 40 * dp : .x += .vx * dp : .y += .vy * dp
          End If
        Case K_BLACKHOLE
          '' Pull grows with p^2 and with 1/(dist+6): weak at first (the alien "notices"), then
          '' overwhelming, and the nearest pieces go first - which is what tidal stretching looks
          '' like. The swirl term (40) keeps the stream from being a straight line. A fragment
          '' that reaches the point is swallowed (removed); the point collapses in Render.
          dx = holes(.hole).x - .x : dy = holes(.hole).y - .y
          dist = Sqr(dx * dx + dy * dy)
          If dist < 2.5 Then .age = .dur + 1
          q = 9000 * p * p / (dist + 6)
          .vx += (dx * q - dy * 40) / dist * dp : .vy += (dy * q + dx * 40) / dist * dp
          .x += .vx * dp : .y += .vy * dp
      End Select
      If .age >= .dur Then frags(i) = frags(nFrags - 1) : nFrags -= 1 Else i += 1
    End With
  Wend
  i = 0
  While i < nGhosts
    ghosts(i).age += dt
    If ghosts(i).age >= ghosts(i).dur Then ghosts(i) = ghosts(nGhosts - 1) : nGhosts -= 1 Else i += 1
  Wend
  '' Holes are indexed by their fragments, so only the LAST one may be retired, and only when done.
  For i = 0 To nHoles - 1 : holes(i).age += dt : Next
  If nHoles > 0 Then If holes(nHoles - 1).age >= holes(nHoles - 1).dur Then nHoles -= 1
End Sub

'' RENDER the fragments. Colour is derived from (spawn colour, p, kind) every frame instead of being
'' stored, so every colour rule lives here and a fragment stays at 14 fields.
Sub RenderFragments()
  Dim As Integer i
  Dim As Single p, r, g, b, k, dx, dy, ln
  For i = 0 To nFrags - 1
    With frags(i)
      p = .age / .dur : r = .r : g = .g : b = .b
      Select Case .kind
        Case K_DISINTEGRATE, K_TORNADO
          '' Linear fade; (1-p)^2 empties the screen too early at short durations.
          r *= 1 - p : g *= 1 - p : b *= 1 - p
        Case K_FREEZE
          '' Towards ice (200,235,255), then towards grey AND dark: the saturation leaves before
          '' the brightness, a melting icicle rather than a fading one.
          k = p / 0.35
          r = Lerp(r, 200, k) : g = Lerp(g, 235, k) : b = Lerp(b, 255, k)
          If p > 0.35 Then
            k = (p - 0.35) / 0.65
            r = Lerp(r, 140, k) * (1 - k) : g = Lerp(g, 140, k) * (1 - k) : b = Lerp(b, 150, k) * (1 - k)
          End If
        Case K_CHAR
          '' The char front: a circle from the impact point, radius growing with p. Inside it the
          '' colour is ember-black; a pixel just inside glows orange for a moment (the 0.15-sprite
          '' band), which is what makes it read as BURNING and not as dimming.
          dx = .x - .ax : dy = .y - .ay
          k = (p * 2.2 * sprW - Sqr(dx * dx + dy * dy)) / (0.15 * sprW)
          If k >= 1 Or p > 0.42 Then       '' sticky: a crumbling fragment has LEFT the front
            r = 35 : g = 15 : b = 10
          ElseIf k > 0 Then
            r = Lerp(r, 255, k) : g = Lerp(g, 120, k) : b = Lerp(b, 20, k)
          End If
          If p > 0.4 Then k = 1 - (p - 0.4) / 0.6 : r *= k : g *= k : b *= k
        Case K_BLACKHOLE
          '' Spaghettification: the fragment is a LINE along its velocity, length growing with
          '' speed (capped at 14 px so the tail never overtakes the hole), colour shifting to
          '' blue-white as it stretches - the visual for "heated by tidal forces".
          dx = .vx : dy = .vy : ln = Sqr(dx * dx + dy * dy)
          k = ln * 0.012 : If k > 14 Then k = 14
          r = Lerp(r, 210, p) : g = Lerp(g, 225, p) : b = Lerp(b, 255, p)
          If ln > 0.01 Then Line (.x, .y)-(.x - dx / ln * k, .y - dy / ln * k), C255(r, g, b) : Continue For
      End Select
      PSet (.x, .y), C255(r, g, b)
    End With
  Next
  For i = 0 To nHoles - 1
    '' The point: an accretion ring while it feeds, a bright collapse in the last 15%.
    p = holes(i).age / holes(i).dur
    If p < 0.85 Then
      Circle (holes(i).x, holes(i).y), 2 + 3 * p, RGB(120, 90, 255)
    Else
      Circle (holes(i).x, holes(i).y), 1 + 6 * (1 - (p - 0.85) / 0.15), RGB(255, 255, 255), , , , F
    End If
  Next
End Sub

'' GHOST - the second code path. Shares with the fragments: Spawn's dispatch, the age/dur clock, Lerp
'' and the sprite table. Shares NOTHING of the state or the motion: no fragment list, the sprite is
'' drawn intact through a colour transform. Wings and halo are derived from the sprite's bounding
'' box (sprW, sprH), so a 24x24 alien gets 24x24 wings.
Sub RenderGhosts()
  Dim As Integer i, x, y, c, gx, gy
  Dim As Single p, a, r, g, b, wr
  For i = 0 To nGhosts - 1
    p = ghosts(i).age / ghosts(i).dur
    '' Rise 70 px per death with a 2.5-cycle, 5 px sway: the sway is what makes it FLOAT; without
    '' it this is a sprite scrolling up. Six cycles reads as shivering.
    gx = ghosts(i).x + Sin(p * 15.7) * 5 : gy = ghosts(i).y - p * 70
    a = 1 - p * p                                 '' opacity holds, then drops: (1-p) went dark by mid-death
    wr = sprW * 0.55                              '' wing radius; aspect breathes: four flaps per death
    '' Wings: filled ellipses beside the body at a third of its brightness; halo: a flat ellipse
    '' above the head, brighter than the body so the eye finds it first.
    c = C255(60 * a, 80 * a, 120 * a)
    Circle (gx - wr * 0.6, gy + sprH * 0.45), wr, c, , , 0.35 + 0.25 * Sin(p * 25), F
    Circle (gx + sprW + wr * 0.6, gy + sprH * 0.45), wr, c, , , 0.35 + 0.25 * Sin(p * 25), F
    Circle (gx + sprW / 2, gy - sprH * 0.3), sprW * 0.35, C255(230 * a, 230 * a, 160 * a), , , 0.3
    For y = 0 To sprH - 1
      For x = 0 To sprW - 1
        c = sprite(x, y)
        If c = 0 Then Continue For
        '' Desaturate towards pale blue-white, then scale by opacity (fake alpha over black).
        r = Lerp((c Shr 16) And 255, 200, p * 1.5) * a
        g = Lerp((c Shr 8) And 255, 225, p * 1.5) * a
        b = Lerp(c And 255, 255, p * 1.5) * a
        '' Soft edge: the four neighbours get 30% of the pixel once p > 0.25, drawn FIRST so the
        '' real pixel wins where both land. Five PSet per pixel - the ghost's whole cost.
        If p > 0.25 Then
          c = C255(r * 0.3, g * 0.3, b * 0.3)
          PSet (gx + x + 1, gy + y), c : PSet (gx + x - 1, gy + y), c
          PSet (gx + x, gy + y + 1), c : PSet (gx + x, gy + y - 1), c
        End If
        PSet (gx + x, gy + y), C255(r, g, b)
      Next
    Next
  Next
End Sub

Sub RenderSprite( ByVal sx As Integer, ByVal sy As Integer )
  Dim As Integer x, y
  For y = 0 To sprH - 1
    For x = 0 To sprW - 1
      If sprite(x, y) <> 0 Then PSet (sx + x, sy + y), sprite(x, y)
    Next
  Next
End Sub

'' Median / p99 / worst over the last STATS_N frames; a 120-element insertion sort per frame is
'' invisible next to the fragments, and a running estimate would hide the spikes.
Sub FrameStats( ByRef med As Double, ByRef p99 As Double, ByRef worst As Double )
  Dim As Double s(0 To STATS_N - 1), t
  Dim As Integer i, j
  For i = 0 To STATS_N - 1
    t = frameMs(i) : j = i - 1
    While j >= 0
      If s(j) <= t Then Exit While
      s(j + 1) = s(j) : j -= 1
    Wend
    s(j + 1) = t
  Next
  med = s(STATS_N \ 2) : p99 = s(STATS_N - 2) : worst = s(STATS_N - 1)
End Sub

Function Fmt( ByVal v As Double ) As String                '' two decimals, integer arithmetic only
  Dim As Integer c = Int(v * 100 + 0.5)
  Return Str(c \ 100) + "." + Right("0" + Str(c Mod 100), 2)
End Function

Function ArgValue( ByVal argName As String, ByVal deflt As String ) As String
  Dim As Integer i
  For i = 1 To 8
    If Left(Command(i), Len(argName) + 1) = argName + "=" Then Return Mid(Command(i), Len(argName) + 2)
  Next
  Return deflt
End Function

Sub SpawnTwenty( ByVal kind As Integer, ByVal dur As Single )
  Dim As Integer i
  For i = 0 To 19 : Spawn(kind, 80 + (i Mod 5) * 110, 100 + (i \ 5) * 90, dur) : Next
End Sub

'' One update+render step, timed; the window loop and the headless runs all call this.
Sub StepFrame( ByVal dt As Single, ByVal sx As Integer, ByVal sy As Integer, ByVal dur As Single )
  Dim As Double t0 = Timer, med, p99, worst
  Line (0, 0)-(SCREEN_W - 1, SCREEN_H - 1), 0, BF
  UpdateFragments(dt)
  RenderSprite(sx, sy) : RenderFragments() : RenderGhosts()
  frameMs(frameIdx Mod STATS_N) = (Timer - t0) * 1000 : frameIdx += 1
  FrameStats(med, p99, worst)
  Draw String (8, SCREEN_H - 30), "duration " + Fmt(dur) + " s   [1-6] effect  [+/-] duration  [S] twenty at once  [Q] quit", RGB(200, 200, 200)
  Draw String (8, SCREEN_H - 16), "frame ms (last 120)  median " + Fmt(med) + "  p99 " + Fmt(p99) + "  worst " + Fmt(worst) + "   fragments " + Str(nFrags) + "  ghosts " + Str(nGhosts), RGB(200, 200, 200)
End Sub

Sub WritePPM( ByVal fileName As String )
  Dim As Integer f = FreeFile, x, y, c
  Dim As String row
  Open fileName For Binary Access Write As #f
  Put #f, , "P6" + Chr(10) + Str(SCREEN_W) + " " + Str(SCREEN_H) + Chr(10) + "255" + Chr(10)
  For y = 0 To SCREEN_H - 1
    row = ""
    For x = 0 To SCREEN_W - 1
      c = Point(x, y) : row += Chr((c Shr 16) And 255) + Chr((c Shr 8) And 255) + Chr(c And 255)
    Next
    Put #f, , row
  Next
  Close #f
End Sub

'' ================================================================================================
''  MAIN.  Arguments (any order): run=window|bench|capture  effect=1..6  dur=<s>  many=1  out=<name>
''  bench: 240 frames at a fixed 60 Hz step, a death every second (twenty with many=1); appends the
''  median / p99 / worst of the last 120 frames, and the mean of all 240, to <out>.txt (in graphics
''  mode fbc's Print goes to the window). capture: one death, stills at p = 0.2 / 0.45 / 0.7 / 0.95.
'' ================================================================================================
Randomize 12345                                 '' fixed seed: the same death on every engine
LoadSprite()
ScreenRes SCREEN_W, SCREEN_H, 32
Dim As Integer sx = SCREEN_W \ 2 - sprW \ 2, sy = SCREEN_H \ 2 - sprH \ 2, i, frames, shot
Dim As Single dur = Val(ArgValue("dur", "0.6"))
Dim As String mode = ArgValue("run", "window"), key
Dim As Integer kind = Val(ArgValue("effect", "1")), many = Val(ArgValue("many", "0"))
Dim As Double med, p99, worst, tPrev, tNow

If mode = "bench" Then
  tPrev = Timer
  For i = 0 To 239
    If i Mod 60 = 0 Then If many Then SpawnTwenty(kind, dur) Else Spawn(kind, sx, sy, dur)
    StepFrame(1.0 / 60, sx, sy, dur)
  Next
  FrameStats(med, p99, worst)
  Open ArgValue("out", "bench") + ".txt" For Append As #1
  Print #1, "effect " + Str(kind) + " dur " + Fmt(dur) + " many " + Str(many) + ": median " + Fmt(med) + " ms  p99 " + Fmt(p99) + " ms  worst " + Fmt(worst) + " ms  mean " + Fmt((Timer - tPrev) * 1000 / 240) + " ms"
  Close #1
  End
ElseIf mode = "capture" Then
  Spawn(kind, sx, sy, dur)
  frames = Int(dur * 60)
  For i = 1 To frames
    StepFrame(1.0 / 60, sx, sy, dur)
    If i = Int(frames * 0.2) Or i = Int(frames * 0.45) Or i = Int(frames * 0.7) Or i = Int(frames * 0.95) Then
      WritePPM(ArgValue("out", "death") + "_" + Str(shot) + ".ppm") : shot += 1
    End If
  Next
  End
End If

tPrev = Timer
Do
  tNow = Timer
  ScreenLock
  StepFrame(tNow - tPrev, sx, sy, dur)
  ScreenUnlock
  tPrev = tNow
  key = Inkey
  Select Case key
    Case "1", "2", "3", "4", "5", "6": kind = Val(key) : Spawn(kind, sx, sy, dur)
    Case "+", "=": dur += DUR_STEP : If dur > DUR_MAX Then dur = DUR_MAX
    Case "-": dur -= DUR_STEP : If dur < DUR_MIN Then dur = DUR_MIN
    Case "s", "S": SpawnTwenty(kind, dur)
    Case "q", "Q", Chr(27): Exit Do
  End Select
  Sleep 1, 1                                    '' yield; without it the loop pins a core
Loop
