'' ================================================================================================
''  CRAZY ALIEN - a wave shooter with a comic tone
'' ================================================================================================
''  A formation of robotic sea creatures advances across the screen, drops a row at every turn and
''  speeds up as it thins; the cannon at the bottom moves sideways and fires; four shields take the
''  hits until they crumble. The wave is lost when the formation reaches the cannon's row.
''
''  What is different is how they DIE. Every death is computed from the sprite's own pixels by the
''  engine in effects.bi, and its DURATION follows the formation's speed: a slow wave gets the
''  elaborate deaths, a fast one gets them short, and past LASER_INTERVAL the cannon fires a laser
''  and every death is the shortest disintegration - the rhythm is never broken by an animation.
''  The black hole is a rare event, never an ordinary hit. The eyes tell the alien's state: normal,
''  ALARMED when a shot passes close or the wave is fast, HIT for a few frames before the death.
''
''  THE FILES: aliens.bi is the sprite bank - replace a creature there and nothing here changes -
''  effects.bi the death engine. Everything is drawn in a logical 320x240 and shown at scale=2.
''
''  Keys: LEFT/RIGHT (or A/D) move, SPACE fires, P pauses, F fullscreen, Q or ESC quits.
''  Arguments: scale=1|2  fullscreen=1  auto=1 (the cannon plays itself)  frames=N (stop after N
''  frames)  out=<name> (write the last frame as <name>.ppm) - the last three are for testing.
'' ================================================================================================
#include "aliens.bi"
#include "effects.bi"

Const LOGICAL_W = 320, LOGICAL_H = 240, GFX_FULLSCREEN_FLAG = 1
Const SC_ESCAPE = 1, SC_SPACE = 57, SC_LEFT = 75, SC_RIGHT = 77, SC_A = 30, SC_D = 32
Const COLS = 5, ROWS = 4, FORM_X0 = 40, FORM_Y0 = 16, COL_GAP = 40, ROW_GAP = 30
Const STEP_PX = 6, DROP_PX = 10
Const START_INTERVAL = 0.55, MIN_INTERVAL = 0.05, LASER_INTERVAL = 0.13   '' seconds per formation step
Const PLAYER_Y = 214, PLAYER_SPEED = 2, SHOT_SPEED = 5, BOMB_SPEED = 2
Const SHIELD_Y = 186, SHIELD_W = 24, SHIELD_H = 14, N_SHIELDS = 4
Const HIT_FRAMES = 6, BLACK_HOLE_CHANCE = 0.05, MAX_BOMBS = 8

Type Alien
  alive As Integer : species As Integer : x As Integer : y As Integer
  eyes As Integer : hitTimer As Integer         '' HIT for a few frames, then the death spawns
End Type
Type Bomb
  x As Single : y As Single : alive As Integer
End Type

Dim Shared As Alien aliens(0 To COLS * ROWS - 1)
Dim Shared As Bomb bombs(0 To MAX_BOMBS - 1)
Dim Shared As Integer shield(0 To N_SHIELDS - 1, 0 To SHIELD_W - 1, 0 To SHIELD_H - 1)
Dim Shared As Integer playerX, playerAlive, playerRespawn, lives, score, wave, aliveCount
Dim Shared As Integer shotX, shotY, shotAlive, laserTimer, laserX, formDir, gameOver, paused
Dim Shared As Single formInterval, formClock, bombClock
Dim Shared As Integer cannonIdx

'' ------------------------------------------------------------------------------------------
''  THE WAVE
'' ------------------------------------------------------------------------------------------
Sub BuildShields()
  Dim As Integer s, x, y
  For s = 0 To N_SHIELDS - 1
    For y = 0 To SHIELD_H - 1
      For x = 0 To SHIELD_W - 1
        '' A bunker: full block with a notch under it, and rounded top corners.
        shield(s, x, y) = 1
        If y > SHIELD_H - 5 And x > 7 And x < SHIELD_W - 8 Then shield(s, x, y) = 0
        If y < 3 And (x < 3 - y Or x > SHIELD_W - 4 + y) Then shield(s, x, y) = 0
      Next
    Next
  Next
End Sub

Sub StartWave( ByVal w As Integer )
  Dim As Integer i, c, r
  wave = w
  For r = 0 To ROWS - 1
    For c = 0 To COLS - 1
      i = r * COLS + c
      With aliens(i)
        .alive = 1 : .species = r Mod BANK_ALIENS
        .x = FORM_X0 + c * COL_GAP : .y = FORM_Y0 + r * ROW_GAP
        .eyes = EYE_NORMAL : .hitTimer = 0
      End With
    Next
  Next
  aliveCount = COLS * ROWS
  '' Each wave starts a little faster than the last; the floor is the laser regime.
  formInterval = START_INTERVAL * (0.85 ^ (w - 1))
  If formInterval < MIN_INTERVAL Then formInterval = MIN_INTERVAL
  formClock = 0 : bombClock = 0 : formDir = 1
  For i = 0 To MAX_BOMBS - 1 : bombs(i).alive = 0 : Next
  shotAlive = 0 : laserTimer = 0
End Sub

'' The formation's speed: the interval shrinks as aliens die (the classic rule), and the DEATH
'' DURATION is read off the same number, so nothing has to be tuned twice.
Function CurrentInterval() As Single
  Dim As Single f = aliveCount / (COLS * ROWS)          '' 1 at the start, ~0 at the end
  Dim As Single t = formInterval * (0.15 + 0.85 * f)
  If t < MIN_INTERVAL Then t = MIN_INTERVAL
  Return t
End Function

Function DeathDuration() As Single
  Dim As Single d = CurrentInterval() * 2.2
  If d < 0.15 Then d = 0.15
  If d > 1.0 Then d = 1.0
  Return d
End Function

Function LaserMode() As Integer
  Return CurrentInterval() <= LASER_INTERVAL
End Function

'' Which death: the shortest disintegration in laser mode; otherwise one of the elaborate ones, and
'' the black hole only rarely and only when there is time for it to read.
Function PickDeath( ByVal dur As Single ) As Integer
  If LaserMode() Then Return K_DISINTEGRATE
  If dur >= 0.5 And Rnd < BLACK_HOLE_CHANCE Then Return K_BLACKHOLE
  Select Case Int(Rnd * 5)
    Case 0: Return K_DISINTEGRATE
    Case 1: Return K_TORNADO
    Case 2: Return K_FREEZE
    Case 3: Return K_CHAR
    Case Else: Return K_GHOST
  End Select
End Function

Sub KillAlien( ByVal i As Integer )
  Dim As Single d = DeathDuration()
  Spawn(PickDeath(d), aliens(i).species, aliens(i).x, aliens(i).y, IIf(LaserMode(), 0.15, d))
  aliens(i).alive = 0
  aliveCount -= 1
  score += 10 * (ROWS - aliens(i).y \ ROW_GAP)
End Sub

Sub StepFormation()
  Dim As Integer i, hitEdge = 0
  For i = 0 To COLS * ROWS - 1
    If aliens(i).alive Then
      If formDir > 0 And aliens(i).x + sprW + STEP_PX > LOGICAL_W - 4 Then hitEdge = 1
      If formDir < 0 And aliens(i).x - STEP_PX < 4 Then hitEdge = 1
    End If
  Next
  If hitEdge Then
    formDir = -formDir
    For i = 0 To COLS * ROWS - 1
      If aliens(i).alive Then aliens(i).y += DROP_PX
      If aliens(i).alive And aliens(i).y + sprH >= PLAYER_Y Then gameOver = 1
    Next
  Else
    For i = 0 To COLS * ROWS - 1
      If aliens(i).alive Then aliens(i).x += formDir * STEP_PX
    Next
  End If
End Sub

Sub DropBomb()
  Dim As Integer i, k, n = 0, pick(0 To COLS * ROWS - 1)
  For i = 0 To COLS * ROWS - 1
    If aliens(i).alive Then pick(n) = i : n += 1
  Next
  If n = 0 Then Exit Sub
  For k = 0 To MAX_BOMBS - 1
    If bombs(k).alive = 0 Then
      i = pick(Int(Rnd * n))
      bombs(k).x = aliens(i).x + sprW / 2 : bombs(k).y = aliens(i).y + sprH : bombs(k).alive = 1
      Exit Sub
    End If
  Next
End Sub

'' ------------------------------------------------------------------------------------------
''  HITS
'' ------------------------------------------------------------------------------------------
Function ShieldHit( ByVal px As Integer, ByVal py As Integer ) As Integer
  '' A projectile at (px, py): is it inside a shield pixel? If so blast a hole of radius 3 there.
  Dim As Integer s, x, y, lx, ly, dx, dy
  For s = 0 To N_SHIELDS - 1
    lx = px - (40 + s * 72) : ly = py - SHIELD_Y
    If lx >= 0 And lx < SHIELD_W And ly >= 0 And ly < SHIELD_H Then
      If shield(s, lx, ly) Then
        For dy = -3 To 3
          For dx = -3 To 3
            x = lx + dx : y = ly + dy
            If x >= 0 And x < SHIELD_W And y >= 0 And y < SHIELD_H And dx * dx + dy * dy <= 10 Then shield(s, x, y) = 0
          Next
        Next
        Return 1
      End If
    End If
  Next
  Return 0
End Function

Function AlienAt( ByVal px As Integer, ByVal py As Integer ) As Integer
  Dim As Integer i
  For i = 0 To COLS * ROWS - 1
    With aliens(i)
      If .alive And .hitTimer = 0 And px >= .x And px < .x + sprW And py >= .y And py < .y + sprH Then Return i
    End With
  Next
  Return -1
End Function

'' The laser: instant, the whole column above the cannon, the first alien it meets is the one hit.
Sub FireLaser()
  Dim As Integer i, best = -1, y
  laserX = playerX + sprW \ 2 : laserTimer = 3
  For i = 0 To COLS * ROWS - 1
    With aliens(i)
      If .alive And .hitTimer = 0 And laserX >= .x And laserX < .x + sprW Then
        '' Two Ifs, not "best < 0 Or": BASIC's Or evaluates both sides, and aliens(-1) is out of bounds.
        If best < 0 Then
          best = i
        ElseIf .y > aliens(best).y Then
          best = i
        End If
      End If
    End With
  Next
  If best >= 0 Then aliens(best).hitTimer = 2 : aliens(best).eyes = EYE_HIT
End Sub

Sub PlayerDies()
  Spawn(K_DISINTEGRATE, cannonIdx, playerX, PLAYER_Y, 0.8)
  playerAlive = 0 : playerRespawn = 90
  lives -= 1
  If lives <= 0 Then gameOver = 1
End Sub

'' ------------------------------------------------------------------------------------------
''  ONE FRAME
'' ------------------------------------------------------------------------------------------
Sub UpdateGame( ByVal dt As Single, ByVal keyLeft As Integer, ByVal keyRight As Integer, ByVal keyFire As Integer )
  Dim As Integer i, k, hit
  If gameOver Or paused Then Exit Sub
  '' the cannon
  If playerAlive Then
    If keyLeft Then playerX -= PLAYER_SPEED
    If keyRight Then playerX += PLAYER_SPEED
    If playerX < 0 Then playerX = 0
    If playerX > LOGICAL_W - sprW Then playerX = LOGICAL_W - sprW
    If keyFire And shotAlive = 0 And laserTimer = 0 Then
      If LaserMode() Then
        FireLaser()
      Else
        shotX = playerX + sprW \ 2 : shotY = PLAYER_Y : shotAlive = 1
      End If
    End If
  Else
    playerRespawn -= 1
    If playerRespawn <= 0 Then playerAlive = 1 : playerX = LOGICAL_W \ 2 - sprW \ 2
  End If
  '' the shot
  If shotAlive Then
    shotY -= SHOT_SPEED
    If shotY < 0 Then shotAlive = 0
    If shotAlive And ShieldHit(shotX, shotY) Then shotAlive = 0
    If shotAlive Then
      hit = AlienAt(shotX, shotY)
      If hit >= 0 Then aliens(hit).hitTimer = HIT_FRAMES : aliens(hit).eyes = EYE_HIT : shotAlive = 0
    End If
    '' the near miss: a shot passing beside an alien alarms it
    If shotAlive Then
      For i = 0 To COLS * ROWS - 1
        With aliens(i)
          If .alive And .hitTimer = 0 And Abs(shotX - (.x + sprW \ 2)) < sprW And Abs(shotY - .y) < sprH * 2 Then .eyes = EYE_ALARMED
        End With
      Next
    End If
  End If
  If laserTimer > 0 Then laserTimer -= 1
  '' the hit aliens die when their flash is over
  For i = 0 To COLS * ROWS - 1
    With aliens(i)
      If .alive And .hitTimer > 0 Then
        .hitTimer -= 1
        If .hitTimer = 0 Then KillAlien(i)
      ElseIf .alive And .eyes = EYE_ALARMED And Rnd < 0.02 Then
        .eyes = EYE_NORMAL                    '' calms down after a while
      End If
      If .alive And LaserMode() Then .eyes = EYE_ALARMED   '' a fast wave is a frightened wave
    End With
  Next
  '' the formation
  formClock += dt
  If formClock >= CurrentInterval() Then formClock = 0 : StepFormation()
  '' the bombs
  bombClock += dt
  If bombClock >= 0.9 * (0.4 + 0.6 * aliveCount / (COLS * ROWS)) Then bombClock = 0 : DropBomb()
  For k = 0 To MAX_BOMBS - 1
    With bombs(k)
      If .alive Then
        .y += BOMB_SPEED
        If .y > LOGICAL_H Then .alive = 0
        If .alive And ShieldHit(Int(.x), Int(.y)) Then .alive = 0
        If .alive And playerAlive And .y >= PLAYER_Y And .y < PLAYER_Y + sprH And .x >= playerX And .x < playerX + sprW Then
          .alive = 0 : PlayerDies()
        End If
      End If
    End With
  Next
  UpdateFragments(dt)
  '' the next wave, once the last death has played out
  If aliveCount = 0 And nFrags = 0 And nGhosts = 0 Then StartWave(wave + 1)
End Sub

Sub RenderGame()
  Dim As Integer i, s, x, y, k
  Line (0, 0)-(LOGICAL_W * scale - 1, LOGICAL_H * scale - 1), 0, BF
  For s = 0 To N_SHIELDS - 1
    For y = 0 To SHIELD_H - 1
      For x = 0 To SHIELD_W - 1
        If shield(s, x, y) Then Plot(40 + s * 72 + x, SHIELD_Y + y, IIf(y < 2, &H8CC8A0, &H46A064))
      Next
    Next
  Next
  For i = 0 To COLS * ROWS - 1
    If aliens(i).alive Then DrawSprite(aliens(i).species, aliens(i).x, aliens(i).y, aliens(i).eyes)
  Next
  If playerAlive Then DrawSprite(cannonIdx, playerX, PLAYER_Y, EYE_NORMAL)
  If shotAlive Then For k = 0 To 3 : Plot(shotX, shotY + k, &HFFF0A0) : Next
  If laserTimer > 0 Then Line (laserX * scale, 0)-(laserX * scale + scale - 1, PLAYER_Y * scale), &H60E0FF, BF
  For k = 0 To MAX_BOMBS - 1
    If bombs(k).alive Then Plot(Int(bombs(k).x), Int(bombs(k).y), &HFF6040) : Plot(Int(bombs(k).x), Int(bombs(k).y) + 1, &HFFB040)
  Next
  RenderFragments() : RenderGhosts()
  Draw String (8, 4), "SCORE " + Str(score) + "   WAVE " + Str(wave) + "   LIVES " + Str(lives) + IIf(LaserMode(), "   LASER", ""), RGB(200, 200, 200)
  If gameOver Then Draw String (LOGICAL_W * scale \ 2 - 40, LOGICAL_H * scale \ 2), "GAME OVER", RGB(255, 80, 80)
  If paused Then Draw String (LOGICAL_W * scale \ 2 - 24, LOGICAL_H * scale \ 2), "PAUSED", RGB(200, 200, 80)
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

Function ArgValue( ByVal argName As String, ByVal deflt As String ) As String
  Dim As Integer i
  For i = 1 To 8
    If Left(Command(i), Len(argName) + 1) = argName + "=" Then Return Mid(Command(i), Len(argName) + 2)
  Next
  Return deflt
End Function

'' The cannon plays itself (auto=1): drift under the nearest living alien and keep firing. For
'' testing the whole game headless, not for playing.
Sub AutoPilot( ByRef keyLeft As Integer, ByRef keyRight As Integer, ByRef keyFire As Integer )
  Dim As Integer i, best = -1
  For i = 0 To COLS * ROWS - 1
    If aliens(i).alive Then
      If best < 0 Then
        best = i
      ElseIf aliens(i).y > aliens(best).y Then
        best = i
      End If
    End If
  Next
  keyLeft = 0 : keyRight = 0 : keyFire = 1
  If best >= 0 Then
    If aliens(best).x > playerX + 2 Then keyRight = 1
    If aliens(best).x < playerX - 2 Then keyLeft = 1
  End If
End Sub

'' ================================================================================================
''  MAIN
'' ================================================================================================
Randomize Timer
LoadSprites()
cannonIdx = SpriteIndex("CANNON")
scale = Val(ArgValue("scale", "2")) : If scale < 1 Then scale = 1
Dim As Integer fullscreen = Val(ArgValue("fullscreen", "0")), auto = Val(ArgValue("auto", "0"))
Dim As Integer maxFrames = Val(ArgValue("frames", "0")), frame = 0
Dim As String outName = ArgValue("out", ""), key
If fullscreen Then ScreenRes LOGICAL_W * scale, LOGICAL_H * scale, 32, 1, GFX_FULLSCREEN_FLAG Else ScreenRes LOGICAL_W * scale, LOGICAL_H * scale, 32, 1, 0
BuildShields()
lives = 3 : score = 0 : playerAlive = 1 : playerX = LOGICAL_W \ 2 - sprW \ 2 : gameOver = 0 : paused = 0
StartWave(1)

Dim As Double tPrev = Timer, tNow, acc = 0
Dim As Integer kL, kR, kF
Do
  tNow = Timer
  acc += tNow - tPrev : tPrev = tNow
  If acc > 0.25 Then acc = 0.25                 '' a stall must not turn into a burst of steps
  '' Fixed 60 Hz steps: the formation clock and the deaths see the same dt on every machine.
  While acc >= 1.0 / 60
    acc -= 1.0 / 60
    If auto Then
      AutoPilot(kL, kR, kF)
    Else
      kL = MultiKey(SC_LEFT) Or MultiKey(SC_A) : kR = MultiKey(SC_RIGHT) Or MultiKey(SC_D) : kF = MultiKey(SC_SPACE)
    End If
    UpdateGame(1.0 / 60, kL, kR, kF)
    frame += 1
  Wend
  ScreenLock : RenderGame() : ScreenUnlock
  key = Inkey
  Select Case key
    Case "q", "Q", Chr(27): Exit Do
    Case "p", "P": paused = 1 - paused
    Case "f", "F": fullscreen = 1 - fullscreen
      If fullscreen Then ScreenRes LOGICAL_W * scale, LOGICAL_H * scale, 32, 1, GFX_FULLSCREEN_FLAG Else ScreenRes LOGICAL_W * scale, LOGICAL_H * scale, 32, 1, 0
    Case Chr(13): If gameOver Then lives = 3 : score = 0 : gameOver = 0 : playerAlive = 1 : BuildShields() : StartWave(1)
  End Select
  If maxFrames > 0 And frame >= maxFrames Then Exit Do
  Sleep 1, 1
Loop
If Len(outName) > 0 Then RenderGame() : WritePPM(outName + ".ppm")
