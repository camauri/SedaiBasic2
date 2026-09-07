''  aliens.bi - the SPRITE BANK of Crazy Alien. Everything drawn as a creature comes from here.
''
''  HOW TO REPLACE A SPRITE. Each sprite is a block of DATA: a header line
''      Data "NAME", "RRGGBB", "RRGGBB"      (the name, then accent colour A and accent colour B)
''  followed by exactly H rows of W characters (three rows per Data line here, any split works).
''  The bank opens with  Data <count>, <W>, <H>  - change the count when adding a sprite, change W
''  and H to use a bigger grid for ALL of them (every sprite in a bank has the same size; the grid
''  can be up to 32x32). Letters of the palette:
''      .  air (transparent)      M  metal        D  dark metal      E  eye / visor
''      P  pupil                  Y  lamp         R  red detail      A  accent 1   B  accent 2
''  The first BANK_ALIENS sprites are the aliens, in the order the formation's rows use them; the
''  sprite named CANNON is the player. Nothing else in the program knows a sprite by its picture:
''  eyes are the E/P pixels, wings and halos come from W and H, deaths take the pixels as they are.
''
''  THE EYES tell the alien's state and are drawn from the same pixels: EYE_NORMAL draws E and P as
''  they are; EYE_ALARMED dilates the pupils (every E pixel touching a P becomes pupil); EYE_HIT
''  flashes the whole eye white-red. Any sprite with E and P pixels gets the three states for free.
Const MAX_SPECIES = 8, EYE_NORMAL = 0, EYE_ALARMED = 1, EYE_HIT = 2
Dim Shared As Integer sprite(0 To MAX_SPECIES - 1, 0 To 31, 0 To 31)   '' 0 = air, else packed RGB
Dim Shared As Integer eyeMask(0 To MAX_SPECIES - 1, 0 To 31, 0 To 31)  '' 0 none, 1 = E, 2 = P
Dim Shared As String spName(0 To MAX_SPECIES - 1)
Dim Shared As Integer sprW, sprH, nSprites, scale
Const PAL_PUPIL = &H0A1428, PAL_EYE = &HC8FFFF, PAL_HIT = &HFF6050

Const BANK_ALIENS = 5              '' the first five sprites are the aliens
Data 6, 24, 24
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
Data "CANNON", "5AC8E6", "F08C28"
Data "...........YY...........", "...........MM...........", "..........DMMD.........."
Data "..........DMMD..........", ".........AMMMMA.........", "........AMMDDMMA........"
Data ".......AMMMMMMMMA.......", "......AMMMEEEEMMMA......", ".....AMMMMEPPEMMMMA....."
Data "....AMMMMMMEEMMMMMMA....", "...AMMMMMMMMMMMMMMMMA...", "..AMMMMMMMMMMMMMMMMMMA.."
Data ".AMMMMMMMMMMMMMMMMMMMMA.", ".ABBBBBBBBBBBBBBBBBBBBA.", ".ADDDDDDDDDDDDDDDDDDDDA."
Data "........................", "........................", "........................"
Data "........................", "........................", "........................"
Data "........................", "........................", "........................"

Sub LoadSprites()
  Dim As Integer x, y, i, k, w, h, pal(0 To 5)
  Dim As String row, hexA, hexB
  pal(0) = &HAAAFB9 : pal(1) = &H464B5A : pal(2) = PAL_EYE : pal(3) = PAL_PUPIL : pal(4) = &HFFDC3C : pal(5) = &HDC2832
  Read nSprites, w, h : sprW = w : sprH = h
  For k = 0 To nSprites - 1
    Read spName(k), hexA, hexB
    For y = 0 To sprH - 1
      Read row
      For x = 0 To sprW - 1
        i = Instr("MDEPYRAB", Mid(row, x + 1, 1))
        If i = 0 Then sprite(k, x, y) = 0
        If i >= 1 And i <= 6 Then sprite(k, x, y) = pal(i - 1)
        If i = 7 Then sprite(k, x, y) = Val("&H" + hexA)
        If i = 8 Then sprite(k, x, y) = Val("&H" + hexB)
        eyeMask(k, x, y) = 0
        If i = 3 Then eyeMask(k, x, y) = 1
        If i = 4 Then eyeMask(k, x, y) = 2
      Next
    Next
  Next
End Sub

Function SpriteIndex( ByVal wanted As String ) As Integer   '' -1 when the bank has no such sprite
  Dim As Integer k
  For k = 0 To nSprites - 1
    If spName(k) = wanted Then Return k
  Next
  Return -1
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

'' Is this E pixel next to a pupil? (the dilation of EYE_ALARMED)
Function NearPupil( ByVal k As Integer, ByVal x As Integer, ByVal y As Integer ) As Integer
  If x > 0 Then If eyeMask(k, x - 1, y) = 2 Then Return 1
  If y > 0 Then If eyeMask(k, x, y - 1) = 2 Then Return 1
  If x < sprW - 1 Then If eyeMask(k, x + 1, y) = 2 Then Return 1
  If y < sprH - 1 Then If eyeMask(k, x, y + 1) = 2 Then Return 1
  Return 0
End Function

Sub DrawSprite( ByVal k As Integer, ByVal sx As Integer, ByVal sy As Integer, ByVal eyes As Integer )
  Dim As Integer x, y, c
  For y = 0 To sprH - 1
    For x = 0 To sprW - 1
      c = sprite(k, x, y)
      If c = 0 Then Continue For
      If eyeMask(k, x, y) > 0 Then
        If eyes = EYE_HIT Then c = PAL_HIT
        If eyes = EYE_ALARMED And eyeMask(k, x, y) = 1 Then If NearPupil(k, x, y) Then c = PAL_PUPIL
      End If
      Plot(sx + x, sy + y, c)
    Next
  Next
End Sub
