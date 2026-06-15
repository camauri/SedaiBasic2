10 REM === SPRDEF - Multi-Sprite Editor & Viewer ===
20 REM Run with sbv.exe. Sprites are shown OVER text mode (as on the C128:
30 REM VIC-IIe sprites display in both text and graphics modes, in front by default).
40 GM = RGR(0) : REM video mode active at RUN time
50 DIM PX(8), PY(8), CO(8), EX(8), EY(8)
60 SS = 1 : REM currently selected sprite
70 REM --- initial layout: 2 rows of 4, colour = sprite number (native 640x400) ---
80 FOR I = 1 TO 8
90 CL = (I - 1) AND 3 : RW = INT((I - 1) / 4)
100 PX(I) = 80 + CL * 140 : PY(I) = 130 + RW * 130
110 CO(I) = I : EX(I) = 1 : EY(I) = 1
120 NEXT I
130 GOSUB 1000 : REM help (startup)
140 GOSUB 2000 : REM edit sprite 1
150 GOSUB 3000 : REM build the viewer (all 8 sprites + label)
160 REM --- VIEWER INPUT LOOP ---
170 GET A$ : IF A$ = "" THEN 170
180 IF A$ = "Q" THEN 800
185 IF A$ = "q" THEN 800
190 IF A$ = "E" THEN GOSUB 2000 : GOSUB 3000 : GOTO 170
195 IF A$ = "e" THEN GOSUB 2000 : GOSUB 3000 : GOTO 170
200 IF A$ = "H" THEN GOSUB 1000 : GOSUB 3000 : GOTO 170
205 IF A$ = "h" THEN GOSUB 1000 : GOSUB 3000 : GOTO 170
210 IF A$ = "C" THEN GOSUB 700 : GOTO 170
215 IF A$ = "c" THEN GOSUB 700 : GOTO 170
216 IF A$ = "S" THEN SPRSAVE "sprites.spr" : GOTO 170
217 IF A$ = "s" THEN SPRSAVE "sprites.spr" : GOTO 170
218 IF A$ = "L" THEN GOSUB 600 : GOTO 170
219 IF A$ = "l" THEN GOSUB 600 : GOTO 170
220 K = VAL(A$) : IF (K >= 1) AND (K <= 8) THEN SS = K : GOSUB 3100 : GOTO 170
230 IF A$ = CHR$(1) THEN PY(SS) = PY(SS) - 16
240 IF A$ = CHR$(2) THEN PY(SS) = PY(SS) + 16
250 IF A$ = CHR$(3) THEN PX(SS) = PX(SS) - 16
260 IF A$ = CHR$(4) THEN PX(SS) = PX(SS) + 16
270 IF PX(SS) < 0 THEN PX(SS) = 0
280 IF PX(SS) > 592 THEN PX(SS) = 592
290 IF PY(SS) < 0 THEN PY(SS) = 0
300 IF PY(SS) > 358 THEN PY(SS) = 358
310 MOVSPR SS, PX(SS), PY(SS)
320 GOTO 170
600 REM --- load sprites from file, then refresh local state & view ---
610 SPRLOAD "sprites.spr", 1 : REM 1 = use the colours saved in the file
620 FOR I = 1 TO 8 : CO(I) = RSPRITE(I, 1) : EX(I) = RSPRITE(I, 3) : EY(I) = RSPRITE(I, 4) : NEXT I
630 GOSUB 3000
640 RETURN
700 REM --- cycle the colour of the selected sprite ---
710 CO(SS) = CO(SS) + 1 : IF CO(SS) > 15 THEN CO(SS) = 0
720 SPRITE SS, 1, CO(SS), 0, EX(SS), EY(SS)
730 RETURN
800 REM --- quit: disable sprites, restore the pre-run video mode ---
810 FOR I = 1 TO 8 : SPRITE I, 0 : NEXT I
820 GRAPHIC GM
830 PRINT "DONE."
840 END
1000 REM === HELP (hides the sprites, shows the keys) ===
1010 FOR I = 1 TO 8 : SPRITE I, 0 : NEXT I
1020 GRAPHIC GM : SCNCLR
1030 PRINT "===== SPRDEF DEMO - HELP ====="
1040 PRINT
1050 PRINT "EDITOR (E opens it on the selected sprite):"
1060 PRINT "  ARROWS  move cursor"
1070 PRINT "  SPACE   paint (toggle / MC cycle)"
1080 PRINT "  0       erase pixel"
1085 PRINT "  1-8     switch sprite being edited"
1090 PRINT "  M       hi-res/multicolor (clears, Y/N)"
1100 PRINT "  X / Y   expand horiz / vert"
1110 PRINT "  C       cycle colour under cursor"
1115 PRINT "  CTRL+C/V copy / paste the shape"
1117 PRINT "  CTRL+S/L save / load file (.spr)"
1118 PRINT "  SHIFT+arr select  CTRL+SPACE pixel"
1119 PRINT "  (SHIFT+CTRL+arr adds an area)"
1121 PRINT "  CTRL+Z/Y undo / redo (per sprite)"
1120 PRINT "  DEL     clear   RETURN save   ESC cancel"
1130 PRINT
1140 PRINT "VIEWER (all 8 sprites shown over text):"
1150 PRINT "  1-8     select a sprite"
1160 PRINT "  ARROWS  move the selected sprite"
1170 PRINT "  C       cycle its colour"
1180 PRINT "  E       edit it   H help   Q quit"
1185 PRINT "  S / L   save / load all sprites (.spr)"
1190 PRINT
1230 PRINT "Press any key..."
1240 GET A$ : IF A$ = "" THEN 1240
1250 RETURN
2000 REM === EDITOR (edit the selected sprite) ===
2010 SPRITE SS, 1, CO(SS)
2020 SPRDEF SS
2030 REM read back ALL colours/expand from the engine (the editor may have
2032 REM changed several sprites, e.g. after a CTRL+L load inside the editor)
2034 FOR J = 1 TO 8 : CO(J) = RSPRITE(J, 1) : EX(J) = RSPRITE(J, 3) : EY(J) = RSPRITE(J, 4) : NEXT J
2040 RETURN
3000 REM === VIEWER SETUP (switch to text, then draw) ===
3010 GRAPHIC 8
3020 GOSUB 3100
3030 RETURN
3100 REM --- draw label + hints, enable & place all 8 sprites ---
3110 SCNCLR
3120 PRINT "SPRITE"; SS; "SELECTED"
3130 PRINT "1-8 select  ARROWS move  C colour"
3140 PRINT "E edit  H help  Q quit"
3150 FOR I = 1 TO 8
3160 SPRITE I, 1, CO(I), 0, EX(I), EY(I) : REM enabled, colour, expand per-sprite
3170 MOVSPR I, PX(I), PY(I)
3180 NEXT I
3190 RETURN
