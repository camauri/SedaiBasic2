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
''
''  THE FILES. aliens.bi is the sprite bank (replace a creature there, nothing else changes) and
''  effects.bi the death engine; both are shared with the game, crazy_alien.bas. This file is the
''  test bench around them.
'' ================================================================================================
#include "aliens.bi"
#include "effects.bi"
Const LOGICAL_W = 320, LOGICAL_H = 240, GFX_FULLSCREEN_FLAG = 1
Const DUR_MIN = 0.05, DUR_MAX = 2.0, DUR_STEP = 0.05, STATS_N = 120
Dim Shared As Integer frameIdx
Dim Shared As Double frameMs(0 To STATS_N - 1)

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
    Spawn(k, i Mod BANK_ALIENS, 20 + (i Mod 5) * 60, 14 + (i \ 5) * 52, dur)
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
Dim Shared As Integer alienAlive = 1, alieBANK_ALIENS = 0
Sub StepFrame( ByVal dt As Single, ByVal sx As Integer, ByVal sy As Integer, ByVal dur As Single )
  Dim As Double t0 = Timer, med, p99, worst
  Dim As Integer sh = LOGICAL_H * scale
  Line (0, 0)-(LOGICAL_W * scale - 1, sh - 1), 0, BF
  UpdateFragments(dt)
  If alienAlive = 0 And nFrags = 0 And nGhosts = 0 Then alienAlive = 1 : alieBANK_ALIENS = (alieBANK_ALIENS + 1) Mod BANK_ALIENS
  If alienAlive Then DrawSprite(alieBANK_ALIENS, sx, sy, EYE_NORMAL)
  RenderFragments() : RenderGhosts()
  frameMs(frameIdx Mod STATS_N) = (Timer - t0) * 1000 : frameIdx += 1
  FrameStats(med, p99, worst)
  Draw String (8, sh - 30), spName(alieBANK_ALIENS) + "   duration " + Fmt(dur) + " s   [1-6] effect  [N] species  [S] twenty  [F] fullscreen  [+/-] duration  [Q] quit", RGB(200, 200, 200)
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
alieBANK_ALIENS = Val(ArgValue("species", "0")) Mod BANK_ALIENS

If mode = "bench" Then
  tPrev = Timer
  For i = 0 To 239
    If i Mod 60 = 0 Then
      If many Then SpawnTwenty(kind, dur) Else Spawn(kind, alieBANK_ALIENS, sx, sy, dur) : alienAlive = 0
    End If
    StepFrame(1.0 / 60, sx, sy, dur)
  Next
  FrameStats(med, p99, worst)
  Open ArgValue("out", "bench") + ".txt" For Append As #1
  Print #1, "effect " + Str(kind) + " scale " + Str(scale) + " dur " + Fmt(dur) + " many " + Str(many) + ": median " + Fmt(med) + " ms  p99 " + Fmt(p99) + " ms  worst " + Fmt(worst) + " ms  mean " + Fmt((Timer - tPrev) * 1000 / 240) + " ms"
  Close #1
  End
ElseIf mode = "capture" Then
  Spawn(kind, alieBANK_ALIENS, sx, sy, dur) : alienAlive = 0
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
      If alienAlive Then Spawn(kind, alieBANK_ALIENS, sx, sy, dur) : alienAlive = 0
    Case "n", "N": If alienAlive Then alieBANK_ALIENS = (alieBANK_ALIENS + 1) Mod BANK_ALIENS
    Case "+", "=": dur += DUR_STEP : If dur > DUR_MAX Then dur = DUR_MAX
    Case "-": dur -= DUR_STEP : If dur < DUR_MIN Then dur = DUR_MIN
    Case "s", "S": SpawnTwenty(-1, dur)
    Case "f", "F": fullscreen = 1 - fullscreen : OpenScreen(fullscreen)
    Case "q", "Q", Chr(27): Exit Do
  End Select
  Sleep 1, 1                                    '' yield; without it the loop pins a core
Loop
