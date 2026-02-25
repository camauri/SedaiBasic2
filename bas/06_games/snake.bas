10 REM =============================================
20 REM  SNAKE GAME - SedaiBasic
30 REM  80x50 text mode, 76x46 centered field
40 REM  Controls: Arrow Keys or W/A/S/D
50 REM  Run with sbv.exe for best experience
60 REM =============================================

100 REM === ONE-TIME INITIALIZATION ===
110 GRAPHIC 8
115 HISCORE = 0
120 DIM SX(200), SY(200)
125 DIM FOODX(20), FOODY(20)
130 SCNCLR
140 PRINT "=== SNAKE GAME ==="
150 PRINT
160 PRINT "Controls:"
170 PRINT "  Arrow Keys - Move snake"
180 PRINT "  Q          - Quit game"
200 PRINT
210 PRINT "Eat the food (*) to grow and score!"
220 PRINT "Speed increases as you eat more."
230 PRINT "Don't hit the walls or yourself!"
240 PRINT: PRINT "Press any key to start..."
250 GETKEY DUMMY$

300 REM =============================================
310 REM  NEW GAME SETUP (restart target)
320 REM =============================================
330 FIELDW = 76    : REM Field width (playable cols 1-76)
340 FIELDH = 46    : REM Field height (playable rows 1-46)
350 MAXLEN = 200   : REM Max snake length
355 MAXFOOD = 10   : REM Max food items on field
360 OX = 1         : REM X offset for centering
370 OY = 1         : REM Y offset for centering
380 BASEFPS = 8    : REM Starting speed
390 MAXFPS = 20    : REM Max speed

500 REM === INITIAL STATE ===
510 SLEN = 3
520 HEADX = 38     : REM Center X of field
530 HEADY = 23     : REM Center Y of field
540 DX = 1 : DY = 0
550 SCORE = 0 : GAMEOVER = 0
560 NFOOD = 1 : FOODSEATEN = 0
570 FPS = BASEFPS

600 REM === INITIALIZE SNAKE BODY ===
610 FOR I = 0 TO SLEN - 1
620 SX(I) = HEADX - I : SY(I) = HEADY
630 NEXT I

700 REM === PLACE INITIAL FOOD ===
710 CURFOOD = 0
720 GOSUB 5000

800 REM === DRAW INITIAL SCREEN ===
810 GOSUB 4000

1000 REM =============================================
1010 REM  MAIN GAME LOOP
1020 REM =============================================
1030 IF GAMEOVER = 1 THEN GOTO 3000
1035 GET K$

1100 REM === PROCESS INPUT ===
1110 IF K$ = "" THEN GOTO 1300
1120 K = ASC(K$)
1130 IF K$ = "W" OR K$ = "w" OR K = 1 THEN IF DY <> 1 THEN DX = 0: DY = -1
1140 IF K$ = "S" OR K$ = "s" OR K = 2 THEN IF DY <> -1 THEN DX = 0: DY = 1
1150 IF K$ = "A" OR K$ = "a" OR K = 3 THEN IF DX <> 1 THEN DX = -1: DY = 0
1160 IF K$ = "D" OR K$ = "d" OR K = 4 THEN IF DX <> -1 THEN DX = 1: DY = 0
1170 IF K$ = "Q" OR K$ = "q" THEN GAMEOVER = 1: GOTO 3000

1300 REM === CALCULATE NEW HEAD POSITION ===
1310 NEWX = SX(0) + DX
1320 NEWY = SY(0) + DY

1400 REM === CHECK WALL COLLISION ===
1410 IF NEWX < 1 OR NEWX > FIELDW THEN GAMEOVER = 1: GOTO 3000
1420 IF NEWY < 1 OR NEWY > FIELDH THEN GAMEOVER = 1: GOTO 3000

1500 REM === CHECK SELF COLLISION ===
1510 FOR I = 0 TO SLEN - 1
1520 IF NEWX = SX(I) AND NEWY = SY(I) THEN GAMEOVER = 1: GOTO 3000
1530 NEXT I

1600 REM === CHECK FOOD COLLISION ===
1610 ATEFOOD = 0 : ATEINDEX = -1
1620 FOR J = 0 TO NFOOD - 1
1630 IF ATEFOOD = 0 AND NEWX = FOODX(J) AND NEWY = FOODY(J) THEN ATEFOOD = 1: ATEINDEX = J
1640 NEXT J
1650 IF ATEFOOD = 0 THEN GOTO 1800
1660 SCORE = SCORE + 10
1670 FOODSEATEN = FOODSEATEN + 1
1680 IF SLEN < MAXLEN THEN SLEN = SLEN + 1
1690 REM Replace eaten food with new one
1700 CURFOOD = ATEINDEX
1710 GOSUB 5000
1720 REM Add more food every 5 eaten
1730 IF NFOOD < MAXFOOD THEN BEGIN
1740   IF FOODSEATEN MOD 5 = 0 THEN BEGIN
1750     CURFOOD = NFOOD
1760     NFOOD = NFOOD + 1
1770     GOSUB 5000
1780   BEND
1790 BEND
1795 REM Update speed
1796 FPS = BASEFPS + INT(FOODSEATEN / 3)
1797 IF FPS > MAXFPS THEN FPS = MAXFPS

1800 REM === MOVE SNAKE ===
1810 IF ATEFOOD = 0 THEN BEGIN
1820   TAILX = SX(SLEN - 1) : TAILY = SY(SLEN - 1)
1830   CHAR 0, TAILX + OX, TAILY + OY, " "
1840 BEND
1850 FOR I = SLEN - 1 TO 1 STEP -1
1860   SX(I) = SX(I - 1) : SY(I) = SY(I - 1)
1870 NEXT I
1880 SX(0) = NEWX : SY(0) = NEWY

1900 REM === DRAW HEAD ===
1910 CHAR 0, NEWX + OX, NEWY + OY, "@"

2000 REM === UPDATE SCORE DISPLAY ===
2010 IF SCORE > HISCORE THEN HISCORE = SCORE
2020 CHAR 0, 2, 0, "SCORE:       "
2030 CHAR 0, 9, 0, STR$(SCORE)
2040 CHAR 0, 56, 0, "HI-SCORE:       "
2050 CHAR 0, 66, 0, STR$(HISCORE)

2200 REM === FRAME SYNC AND LOOP ===
2210 FRAME FPS
2220 GOTO 1030

3000 REM =============================================
3010 REM  GAME OVER
3020 REM =============================================
3030 IF SCORE > HISCORE THEN HISCORE = SCORE
3040 CHAR 0, 31, 20, "*** GAME OVER ***"
3050 CHAR 0, 30, 22, "Score:      "
3060 CHAR 0, 37, 22, STR$(SCORE)
3070 CHAR 0, 30, 24, "High Score: "
3080 CHAR 0, 42, 24, STR$(HISCORE)
3090 CHAR 0, 30, 26, "Play again? (Y/N)"
3100 GETKEY PA$
3110 IF PA$ = "Y" OR PA$ = "y" THEN GOTO 300
3120 IF PA$ <> "N" AND PA$ <> "n" THEN GOTO 3100
3130 SCNCLR
3140 PRINT "Thanks for playing SNAKE!"
3150 PRINT "High score: "; HISCORE
3160 END

4000 REM =============================================
4010 REM  DRAW GAME FIELD (76x46 centered on 80x50)
4020 REM =============================================
4030 SCNCLR
4040 REM Score/HI header on row 0
4050 CHAR 0, 2, 0, "SCORE: 0"
4060 CHAR 0, 56, 0, "HI-SCORE: "
4070 CHAR 0, 66, 0, STR$(HISCORE)
4080 REM Top border (row 1, cols 1-78)
4090 FOR X = 1 TO 78
4100 CHAR 0, X, 1, "#"
4110 NEXT X
4120 REM Side borders (rows 2-47)
4130 FOR Y = 2 TO 47
4140 CHAR 0, 1, Y, "#"
4150 CHAR 0, 78, Y, "#"
4160 NEXT Y
4170 REM Bottom border (row 48, cols 1-78)
4180 FOR X = 1 TO 78
4190 CHAR 0, X, 48, "#"
4200 NEXT X
4210 REM Draw initial snake
4220 FOR I = 0 TO SLEN - 1
4230 IF I = 0 THEN CH$ = "@" ELSE CH$ = "O"
4240 CHAR 0, SX(I) + OX, SY(I) + OY, CH$
4250 NEXT I
4260 REM Draw all food items
4270 FOR J = 0 TO NFOOD - 1
4280 CHAR 0, FOODX(J) + OX, FOODY(J) + OY, "*"
4290 NEXT J
4300 RETURN

5000 REM =============================================
5010 REM  PLACE FOOD AT INDEX CURFOOD
5020 REM =============================================
5030 FOODX(CURFOOD) = INT(RND(1) * FIELDW) + 1
5040 FOODY(CURFOOD) = INT(RND(1) * FIELDH) + 1
5050 REM Check not on snake
5060 FOR I = 0 TO SLEN - 1
5070 IF FOODX(CURFOOD) = SX(I) AND FOODY(CURFOOD) = SY(I) THEN GOTO 5030
5080 NEXT I
5090 REM Check not on other food
5100 FOR I = 0 TO NFOOD - 1
5110 IF I <> CURFOOD AND FOODX(CURFOOD) = FOODX(I) AND FOODY(CURFOOD) = FOODY(I) THEN GOTO 5030
5120 NEXT I
5130 REM Draw food
5140 CHAR 0, FOODX(CURFOOD) + OX, FOODY(CURFOOD) + OY, "*"
5150 RETURN
