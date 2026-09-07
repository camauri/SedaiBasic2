''  effects.bi - the DEATH ENGINE of Crazy Alien: five force fields over a list of fragments, and
''  the ghost. Needs aliens.bi (the sprites, Plot, scale) included before it. The rules and the
''  reasons are at each field; the duration rule is in the prototype's header (deaths.bas).
Const MAX_FRAGS = 16000, MAX_GHOSTS = 24, MAX_HOLES = 24
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
Dim Shared As Integer nFrags, nGhosts, nHoles

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
    '' The point is the alien's OWN centre: the death is the alien collapsing into itself. With the
    '' point on the sprite the radial stretch alone would only shrink the picture, so the swirl
    '' term in Update is what makes it read - the fragments spiral in, stretched along the spiral.
    If nHoles >= MAX_HOLES Then Exit Sub
    h = nHoles : nHoles += 1
    holes(h).x = cx : holes(h).y = cy : holes(h).age = 0 : holes(h).dur = dur
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
          '' like. The swirl (120) is what turns a collapse into a VORTEX now that the point is
          '' the alien's own centre: at 40 the sprite merely shrank. A fragment that reaches the
          '' point is swallowed (removed); the point collapses in Render.
          dx = holes(.hole).x - .x : dy = holes(.hole).y - .y
          dist = Sqr(dx * dx + dy * dy)
          If dist < 1.5 Then .age = .dur + 1
          q = 9000 * p * p / (dist + 6)
          .vx += (dx * q - dy * 120) / dist * dp : .vy += (dy * q + dx * 120) / dist * dp
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

