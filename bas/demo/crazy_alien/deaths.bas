'' ================================================================================================
''  CRAZY ALIEN - procedural death effects, an exploratory prototype
'' ================================================================================================
''  One question: do computed deaths (no hand-drawn animation) hold up visually at ANY speed and
''  inside the frame budget? No game, no movement, no weapon, no score - only the deaths.
''
''  THE PRINCIPLE. Five effects share one road: the sprite's pixels become independent FRAGMENTS
''  (position, velocity, colour) and each effect is a different force field over that list. The
''  sprites are DATA - five robotic sea creatures, 24x24 - and any sprite works. The sixth effect
''  (ghost) is the exception: it transforms the INTACT sprite, a second code path - see GHOST.
''
''  THE DURATION RULE. Every death takes a duration D and must read at every D, so all motion is
''  written in NORMALISED time p = age / D (0 at the hit, 1 at the end): velocities are "pixels per
''  whole death", accelerations "pixels per death squared". The trajectory is the SAME shape at any
''  D - a short D only plays it faster. Nothing has a duration of its own baked in.
''
''  THE VIEWPORT. Everything is drawn in a LOGICAL 320x240 space and shown SCALE times larger (2 by
''  default: each logical pixel is a 2x2 block), because the game will run at a chunky resolution on
''  a large screen. scale=1 gives the native size; fullscreen=1 (or the F key) asks the driver for
''  the whole monitor with GFX_FULLSCREEN, the flag FreeBASIC's fbgfx.bi defines as 1.
''
''  Keys: 1-6 kill the alien with that effect, N next species, S twenty at once (mixed species and
''  effects), F fullscreen, + / - duration, Q quit. Headless runs (bench, capture) are described at
''  MAIN. Transparency is FAKED: the background is black, so "alpha a" is drawn as colour * a.
'' ================================================================================================
Const LOGICAL_W = 320, LOGICAL_H = 240, GFX_FULLSCREEN_FLAG = 1
Const DUR_MIN = 0.05, DUR_MAX = 2.0, DUR_STEP = 0.05
Const MAX_FRAGS = 16000, MAX_GHOSTS = 24, MAX_HOLES = 24, STATS_N = 120, MAX_SPECIES = 8
Const K_DISINTEGRATE = 1, K_TORNADO = 2, K_FREEZE = 3, K_CHAR = 4, K_BLACKHOLE = 5, K_GHOST = 6

Type Fragment
  x As Single : y As Single : vx As Single : vy As Single
  r As Single : g As Single : b As Single          '' colour at spawn; Render derives the rest
  age As Single : dur As Single : seed As Single   '' seed: one random per fragment, fixed at spawn
  kind As Integer : hole As Integer                '' hole: index of the black hole that owns it
  ax As Single : ay As Single                      '' anchor: sprite centre, or the impact point
End Type
Type Floater                                        '' a ghost (with its species), or a hole's point
  x As Single : y As Single : age As Single : dur As Single : species As Integer
End Type

Dim Shared As Fragment frags(0 To MAX_FRAGS - 1)
Dim Shared As Floater ghosts(0 To MAX_GHOSTS - 1), holes(0 To MAX_HOLES - 1)
Dim Shared As Integer nFrags, nGhosts, nHoles, sprW, sprH, nSpecies, frameIdx, scale
Dim Shared As Integer sprite(0 To MAX_SPECIES - 1, 0 To 31, 0 To 31)   '' 0 = air, else packed RGB
Dim Shared As String spName(0 To MAX_SPECIES - 1)
Dim Shared As Double frameMs(0 To STATS_N - 1)

'' THE SPRITES. Shared palette letters: M metal, D dark metal, E visor, P pupil, Y lamp, R red;
'' A and B are the two ACCENT colours each species names in its header ("name", A, B as hex).
'' The count and the size come first, so a sixth alien or a 32x32 one is a data change only.
Data 5, 24, 24
Data "CUTTLEFISH", "C85AE6", "6E3C96"
Data "........AAAAAAAA........", "......AAMMMMMMMMAA......", ".....AMMMMMMMMMMMMA....."
Data "....AMMDMMMMMMMMDMMA....", "...AMMEEDMMMMMMDEEMMA...", "...AMEPPEMMMMMMEPPEMA..."
Data "...AMEPPEMMDDMMEPPEMA...", "...AMMEEMMDYYDMMEEMMA...", "....AMMMMMDYYDMMMMMA...."
Data ".....AMMMMMDDMMMMMA.....", "......AAMMMMMMMMAA......", "........DDDDDDDD........"
Data ".......DBDBDDBDBD.......", ".......B.B.B..B.B.B.....", "......B..B.B..B.B..B...."
Data "......B..B.B..B.B..B....", ".....B...B.B..B.B...B...", ".....B..B..B..B..B..B..."
Data "....B...B..B..B..B...B..", "....B...B..B..B..B...B..", "....B..B...B..B...B..B.."
Data "...B...B...B..B...B...B.", ".......B...B..B...B.....", "........................"
Data "OCTOPUS", "E65046", "962828"
Data ".........MMMMMM.........", ".......MMMMMMMMMM.......", "......MMMDMMMMDMMM......"
Data ".....MMMMMMMMMMMMMM.....", ".....MMEEEEMMEEEEMM.....", "....MMMEPPEMMEPPEMMM...."
Data "....MMMEPPEMMEPPEMMM....", "....MMMMEEMMMMEEMMMM....", "....MMMMMMMDDMMMMMMM...."
Data ".....MMMMMDYYDMMMMM.....", ".....AMMMMMDDMMMMMA.....", "....AAAAAAAAAAAAAAAA...."
Data "...AABABABAABAABABAA....", "..AA.B.B.B.AA.B.B.B.AA..", ".A...B.B.B..A.B.B.B...A."
Data ".A...B.B.B..A.B.B.B...A.", "A...B..B.B..A.B.B..B...A", "A...B..B.B..A.B.B..B...A"
Data "A..B...B..B.A.B..B..B..A", "A..B..B...B...B...B..B.A", ".A.B..B...B...B...B..B.A"
Data ".AB...B...B...B...B...BA", "..B..B....B...B....B..B.", "........................"
Data "CRAB", "F08C28", "AA5014"
Data "....EE............EE....", "...EPPE..........EPPE...", "...EPPE..........EPPE..."
Data "....EE............EE....", ".....D............D.....", ".....D............D....."
Data "AAA..DMMMMMMMMMMMMD..AAA", "A.AA.MMMMMMMMMMMMMM.AA.A", "A..AAMMDMMMMMMMMDMMAA..A"
Data "AA.AMMMMMMMMMMMMMMMMA.AA", ".AAAMMMMMMDYYDMMMMMMAAA.", "..AAMMMMMMMDDMMMMMMMAA.."
Data "...AMMMMMMMMMMMMMMMMA...", "....MMMMMRRRRRRMMMMM....", ".....MMMMMMMMMMMMMM....."
Data "......DDDDDDDDDDDD......", "....BB..B..BB..B..BB....", "...B....B..B.B.B....B..."
Data "..B....B...B.B..B....B..", "..B....B..B...B..B...B..", ".B....B...B...B...B...B."
Data ".B....B..B.....B..B...B.", "B....B...B.....B...B...B", "........................"
Data "HERMIT", "C89050", "3CB4AA"
Data "..........BBBBBBBB......", "........BBBBBBBBBBBB....", ".......BBBBBBBBBBBBBB..."
Data "......BBBBBBBBBBBBBBBB..", ".....BBBBBBDDDDDBBBBBBB.", ".....BBBBBDBBBBBDBBBBBB."
Data "....BBBBBDBBBBBBBDBBBBB.", "....BBBBBDBBDDDBBDBBBBB.", "....BBBBBDBBDBDBBDBBBBB."
Data ".EE.BBBBBBDBBDBBBDBBBBB.", "EPPE.BBBBBBDDDDDDBBBBBB.", "EPPE..BBBBBBBBBBBBBBBB.."
Data ".EE....BBBBBBBBBBBBBB...", "..D.....BBBBBBBBBBBB....", "..DMMMMMMMMBBBBBBBB....."
Data ".MMMMMMMMMMMMMMMMM......", "MMDMMMMMMMMMMMMMM.......", "MMMMMYYMMMMMMMMMM......."
Data "AMMMMMMMMMMMMMMM........", "AA.MMMMMMMMMMMM.........", "A..A.A..A..A..A........."
Data "..A..A.A...A...A........", ".A...A.A..A.....A.......", "........................"
Data "JELLYFISH", "5AC8E6", "3C78C8"
Data ".........AAAAAA.........", ".......AAMMMMMMAA.......", "......AMMMMMMMMMMA......"
Data ".....AMMMDMMMMDMMMA.....", "....AMMMMMMMMMMMMMMA....", "....AMMEEEMMMMEEEMMA...."
Data "...AMMMEPPEMMMEPPEMMMA..", "...AMMMEPPEMMMEPPEMMMA..", "...AMMMMEEMMMMMEEMMMMA.."
Data "...AMMMMMMMDYYDMMMMMMA..", "....AMMMMMMDYYDMMMMMA...", "....AAMMMMMMDDMMMMMAA..."
Data ".....AAAAAAAAAAAAAAA....", "....B.B.B.B..B.B.B.B....", "....B.B.B.B..B.B.B.B...."
Data "...B..B.B.B..B.B.B..B...", "...B..B..B....B..B..B...", "...B..B..B....B..B..B..."
Data "..B..B...B....B...B..B..", "..B..B..B......B..B..B..", "..B.B...B......B...B.B.."
Data ".B..B..B........B..B..B.", ".B.B...B........B...B.B.", "........................"

Sub LoadSprites()
  Dim As Integer x, y, i, k, w, h, pal(0 To 5)
  Dim As String row, hexA, hexB
  pal(0) = &HAAAFB9 : pal(1) = &H464B5A : pal(2) = &HC8FFFF : pal(3) = &H0A1428 : pal(4) = &HFFDC3C : pal(5) = &HDC2832
  Read nSpecies, w, h : sprW = w : sprH = h
  For k = 0 To nSpecies - 1
    Read spName(k), hexA, hexB
    For y = 0 To sprH - 1
      Read row
      For x = 0 To sprW - 1
        i = Instr("MDEPYRAB", Mid(row, x + 1, 1))
        If i = 0 Then sprite(k, x, y) = 0
        If i >= 1 And i <= 6 Then sprite(k, x, y) = pal(i - 1)
        If i = 7 Then sprite(k, x, y) = Val("&H" + hexA)
        If i = 8 Then sprite(k, x, y) = Val("&H" + hexB)
      Next
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

'' One LOGICAL pixel: a scale x scale block of PSets. PSet rather than a filled Line because PSet is
'' the primitive every engine runs natively; a Line per fragment would be the cost, not the effect.
Sub Plot( ByVal x As Integer, ByVal y As Integer, ByVal c As Integer )
  Dim As Integer i, j, bx = x * scale, by = y * scale
  For j = 0 To scale - 1
    For i = 0 To scale - 1
      PSet (bx + i, by + j), c
    Next
  Next
End Sub

'' SPAWN - one call per death. This dispatcher is the ONLY code the two paths share, besides the
'' age/dur clock, Lerp and the sprite table.
Sub Spawn( ByVal kind As Integer, ByVal species As Integer, ByVal sx As Integer, ByVal sy As Integer, ByVal dur As Single )
  Dim As Integer x, y, c, h = -1
  Dim As Single cx = sx + sprW / 2, cy = sy + sprH / 2
  If kind = K_GHOST Then
    If nGhosts >= MAX_GHOSTS Then Exit Sub
    With ghosts(nGhosts)
      .x = sx : .y = sy : .age = 0 : .dur = dur : .species = species
    End With
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
      c = sprite(species, x, y)
      If c = 0 Or nFrags >= MAX_FRAGS Then Continue For
      With frags(nFrags)
        .x = sx + x : .y = sy + y : .vx = 0 : .vy = 0
        .r = (c Shr 16) And 255 : .g = (c Shr 8) And 255 : .b = c And 255
        .age = 0 : .dur = dur : .seed = Rnd : .kind = kind : .hole = h : .ax = cx : .ay = cy
        If kind = K_DISINTEGRATE Then
          '' Radial burst with an upward bias: a symmetric burst reads as a "pop", the bias as a
          '' "blast". 180 px/death sideways, ~7 sprite heights, found by eye; below ~80 the cloud
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
          If ln > 0.01 Then Line (.x * scale, .y * scale)-((.x - dx / ln * k) * scale, (.y - dy / ln * k) * scale), C255(r, g, b) : Continue For
      End Select
      Plot(.x, .y, C255(r, g, b))
    End With
  Next
  For i = 0 To nHoles - 1
    '' The point: an accretion ring while it feeds, a bright collapse in the last 15%.
    p = holes(i).age / holes(i).dur
    If p < 0.85 Then
      Circle (holes(i).x * scale, holes(i).y * scale), (2 + 3 * p) * scale, RGB(120, 90, 255)
    Else
      Circle (holes(i).x * scale, holes(i).y * scale), (1 + 6 * (1 - (p - 0.85) / 0.15)) * scale, RGB(255, 255, 255), , , , F
    End If
  Next
End Sub

'' GHOST - the second code path. Shares with the fragments: Spawn's dispatch, the age/dur clock, Lerp
'' and the sprite table. Shares NOTHING of the state or the motion: no fragment list, the sprite is
'' drawn intact through a colour transform. Wings and halo are derived from the sprite's bounding
'' box (sprW, sprH), so a 32x32 alien gets 32x32 wings.
Sub RenderGhosts()
  Dim As Integer i, x, y, c, gx, gy, k
  Dim As Single p, a, r, g, b, wr
  For i = 0 To nGhosts - 1
    p = ghosts(i).age / ghosts(i).dur : k = ghosts(i).species
    '' Rise 70 px per death with a 2.5-cycle, 5 px sway: the sway is what makes it FLOAT; without
    '' it this is a sprite scrolling up. Six cycles reads as shivering.
    gx = ghosts(i).x + Sin(p * 15.7) * 5 : gy = ghosts(i).y - p * 70
    a = 1 - p * p                                 '' opacity holds, then drops: (1-p) went dark by mid-death
    wr = sprW * 0.55                              '' wing radius; aspect breathes: four flaps per death
    '' Wings: filled ellipses beside the body at a third of its brightness; halo: a flat ellipse
    '' above the head, brighter than the body so the eye finds it first.
    c = C255(60 * a, 80 * a, 120 * a)
    Circle ((gx - wr * 0.6) * scale, (gy + sprH * 0.45) * scale), wr * scale, c, , , 0.35 + 0.25 * Sin(p * 25), F
    Circle ((gx + sprW + wr * 0.6) * scale, (gy + sprH * 0.45) * scale), wr * scale, c, , , 0.35 + 0.25 * Sin(p * 25), F
    Circle ((gx + sprW / 2) * scale, (gy - sprH * 0.3) * scale), sprW * 0.35 * scale, C255(230 * a, 230 * a, 160 * a), , , 0.3
    For y = 0 To sprH - 1
      For x = 0 To sprW - 1
        c = sprite(k, x, y)
        If c = 0 Then Continue For
        '' Desaturate towards pale blue-white, then scale by opacity (fake alpha over black).
        r = Lerp((c Shr 16) And 255, 200, p * 1.5) * a
        g = Lerp((c Shr 8) And 255, 225, p * 1.5) * a
        b = Lerp(c And 255, 255, p * 1.5) * a
        '' Soft edge: the four neighbours get 30% of the pixel once p > 0.25, drawn FIRST so the
        '' real pixel wins where both land. Five plots per pixel - the ghost's whole cost.
        If p > 0.25 Then
          c = C255(r * 0.3, g * 0.3, b * 0.3)
          Plot(gx + x + 1, gy + y, c) : Plot(gx + x - 1, gy + y, c)
          Plot(gx + x, gy + y + 1, c) : Plot(gx + x, gy + y - 1, c)
        End If
        Plot(gx + x, gy + y, C255(r, g, b))
      Next
    Next
  Next
End Sub

Sub RenderSprite( ByVal species As Integer, ByVal sx As Integer, ByVal sy As Integer )
  Dim As Integer x, y
  For y = 0 To sprH - 1
    For x = 0 To sprW - 1
      If sprite(species, x, y) <> 0 Then Plot(sx + x, sy + y, sprite(species, x, y))
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

'' Twenty at once: a 5x4 grid of aliens, every species in turn. sameKind < 0 gives each a random
'' effect (the show); sameKind = k gives them all effect k (the worst case the bench measures).
Sub SpawnTwenty( ByVal sameKind As Integer, ByVal dur As Single )
  Dim As Integer i, k
  For i = 0 To 19
    If sameKind > 0 Then k = sameKind Else k = Int(Rnd * 6) + 1
    Spawn(k, i Mod nSpecies, 20 + (i Mod 5) * 60, 14 + (i \ 5) * 52, dur)
  Next
End Sub

Sub OpenScreen( ByVal fullscreen As Integer )
  If fullscreen Then
    ScreenRes LOGICAL_W * scale, LOGICAL_H * scale, 32, 1, GFX_FULLSCREEN_FLAG
  Else
    ScreenRes LOGICAL_W * scale, LOGICAL_H * scale, 32, 1, 0
  End If
End Sub

'' One update+render step, timed; the window loop and the headless runs all call this. The alien in
'' the middle is drawn only while ALIVE: a death replaces it with its fragments (or its ghost) at the
'' moment of the hit, and it comes back - as the next species - once nothing of the death is left.
Dim Shared As Integer alienAlive = 1, alienSpecies = 0
Sub StepFrame( ByVal dt As Single, ByVal sx As Integer, ByVal sy As Integer, ByVal dur As Single )
  Dim As Double t0 = Timer, med, p99, worst
  Dim As Integer sh = LOGICAL_H * scale
  Line (0, 0)-(LOGICAL_W * scale - 1, sh - 1), 0, BF
  UpdateFragments(dt)
  If alienAlive = 0 And nFrags = 0 And nGhosts = 0 Then alienAlive = 1 : alienSpecies = (alienSpecies + 1) Mod nSpecies
  If alienAlive Then RenderSprite(alienSpecies, sx, sy)
  RenderFragments() : RenderGhosts()
  frameMs(frameIdx Mod STATS_N) = (Timer - t0) * 1000 : frameIdx += 1
  FrameStats(med, p99, worst)
  Draw String (8, sh - 30), spName(alienSpecies) + "   duration " + Fmt(dur) + " s   [1-6] effect  [N] species  [S] twenty  [F] fullscreen  [+/-] duration  [Q] quit", RGB(200, 200, 200)
  Draw String (8, sh - 16), "frame ms (last 120)  median " + Fmt(med) + "  p99 " + Fmt(p99) + "  worst " + Fmt(worst) + "   fragments " + Str(nFrags) + "  ghosts " + Str(nGhosts) + "   scale " + Str(scale), RGB(200, 200, 200)
End Sub

Sub WritePPM( ByVal fileName As String )
  Dim As Integer f = FreeFile, x, y, c, w = LOGICAL_W * scale, h = LOGICAL_H * scale
  Dim As String row
  Open fileName For Binary Access Write As #f
  Put #f, , "P6" + Chr(10) + Str(w) + " " + Str(h) + Chr(10) + "255" + Chr(10)
  For y = 0 To h - 1
    row = ""
    For x = 0 To w - 1
      c = Point(x, y) : row += Chr((c Shr 16) And 255) + Chr((c Shr 8) And 255) + Chr(c And 255)
    Next
    Put #f, , row
  Next
  Close #f
End Sub

'' ================================================================================================
''  MAIN.  Arguments (any order): run=window|bench|capture  effect=1..6  species=0..4  dur=<s>
''         scale=1|2  fullscreen=1  many=1  out=<name>
''  bench: 240 frames at a fixed 60 Hz step, a death every second (twenty of the SAME effect with
''  many=1); appends the median / p99 / worst of the last 120 frames, and the mean of all 240, to
''  <out>.txt (in graphics mode fbc's Print goes to the window). capture: one death, stills at
''  p = 0.2 / 0.45 / 0.7 / 0.95 as <out>_<n>.ppm.
'' ================================================================================================
Randomize 12345                                 '' fixed seed: the same death on every engine
LoadSprites()
scale = Val(ArgValue("scale", "2")) : If scale < 1 Then scale = 1
Dim As Integer fullscreen = Val(ArgValue("fullscreen", "0"))
OpenScreen(fullscreen)
Dim As Integer sx = LOGICAL_W \ 2 - sprW \ 2, sy = LOGICAL_H \ 2 - sprH \ 2, i, frames, shot
Dim As Single dur = Val(ArgValue("dur", "0.6"))
Dim As String mode = ArgValue("run", "window"), key
Dim As Integer kind = Val(ArgValue("effect", "1")), many = Val(ArgValue("many", "0"))
Dim As Double med, p99, worst, tPrev, tNow
alienSpecies = Val(ArgValue("species", "0")) Mod nSpecies

If mode = "bench" Then
  tPrev = Timer
  For i = 0 To 239
    If i Mod 60 = 0 Then
      If many Then SpawnTwenty(kind, dur) Else Spawn(kind, alienSpecies, sx, sy, dur) : alienAlive = 0
    End If
    StepFrame(1.0 / 60, sx, sy, dur)
  Next
  FrameStats(med, p99, worst)
  Open ArgValue("out", "bench") + ".txt" For Append As #1
  Print #1, "effect " + Str(kind) + " scale " + Str(scale) + " dur " + Fmt(dur) + " many " + Str(many) + ": median " + Fmt(med) + " ms  p99 " + Fmt(p99) + " ms  worst " + Fmt(worst) + " ms  mean " + Fmt((Timer - tPrev) * 1000 / 240) + " ms"
  Close #1
  End
ElseIf mode = "capture" Then
  Spawn(kind, alienSpecies, sx, sy, dur) : alienAlive = 0
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
    Case "1", "2", "3", "4", "5", "6"
      kind = Val(key)
      If alienAlive Then Spawn(kind, alienSpecies, sx, sy, dur) : alienAlive = 0
    Case "n", "N": If alienAlive Then alienSpecies = (alienSpecies + 1) Mod nSpecies
    Case "+", "=": dur += DUR_STEP : If dur > DUR_MAX Then dur = DUR_MAX
    Case "-": dur -= DUR_STEP : If dur < DUR_MIN Then dur = DUR_MIN
    Case "s", "S": SpawnTwenty(-1, dur)
    Case "f", "F": fullscreen = 1 - fullscreen : OpenScreen(fullscreen)
    Case "q", "Q", Chr(27): Exit Do
  End Select
  Sleep 1, 1                                    '' yield; without it the loop pins a core
Loop
