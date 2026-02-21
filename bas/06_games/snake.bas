10 REM =============================================
20 REM  SNAKE GAME - SedaiBasic
30 REM  Classic snake game with game loop
40 REM  Controls: W/A/S/D or Arrow Keys
50 REM  Run with sbv.exe for best experience
60 REM =============================================

100 REM === INITIALIZATION ===
110 SCNCLR
120 PRINT "=== SNAKE GAME ==="
130 PRINT
140 PRINT "Controls:"
150 PRINT "  W or UP    - Move up"
160 PRINT "  S or DOWN  - Move down"
170 PRINT "  A or LEFT  - Move left"
180 PRINT "  D or RIGHT - Move right"
190 PRINT "  Q          - Quit game"
200 PRINT
210 PRINT "Eat the food (*) to grow!"
220 PRINT "Don't hit the walls or yourself!"
230 PRINT
240 INPUT "Press ENTER to start..."; DUMMY$

300 REM === GAME CONSTANTS ===
310 FIELDW = 30   : REM Field width
320 FIELDH = 15   : REM Field height
330 MAXLEN = 100  : REM Max snake length
340 SPEED = 0.15  : REM Game speed (seconds per move)

400 REM === ARRAYS ===
410 DIM SX(MAXLEN), SY(MAXLEN)  : REM Snake body positions

500 REM === INITIAL STATE ===
510 SLEN = 3      : REM Snake length
520 HEADX = 15    : REM Head X position
530 HEADY = 7     : REM Head Y position
540 DX = 1        : REM Direction X (1=right, -1=left, 0=none)
550 DY = 0        : REM Direction Y (1=down, -1=up, 0=none)
560 SCORE = 0     : REM Player score
570 GAMEOVER = 0  : REM Game over flag

600 REM === INITIALIZE SNAKE BODY ===
610 FOR I = 0 TO SLEN - 1
620 SX(I) = HEADX - I
630 SY(I) = HEADY
640 NEXT I

700 REM === PLACE FIRST FOOD ===
710 GOSUB 5000

800 REM === DRAW INITIAL SCREEN ===
810 GOSUB 4000

1000 REM =============================================
1010 REM  MAIN GAME LOOP
1020 REM =============================================
1030 IF GAMEOVER = 1 THEN GOTO 3000

1100 REM === GET INPUT (Non-blocking) ===
1110 GET K$
1120 IF K$ = "" THEN GOTO 1200

1130 REM === PROCESS INPUT ===
1140 K = ASC(K$)
1150 REM W or Up arrow
1160 IF K$ = "W" OR K$ = "w" OR K = 145 THEN IF DY <> 1 THEN DX = 0: DY = -1
1170 REM S or Down arrow
1180 IF K$ = "S" OR K$ = "s" OR K = 17 THEN IF DY <> -1 THEN DX = 0: DY = 1
1190 REM A or Left arrow
1200 IF K$ = "A" OR K$ = "a" OR K = 157 THEN IF DX <> 1 THEN DX = -1: DY = 0
1210 REM D or Right arrow
1220 IF K$ = "D" OR K$ = "d" OR K = 29 THEN IF DX <> -1 THEN DX = 1: DY = 0
1230 REM Q to quit
1240 IF K$ = "Q" OR K$ = "q" THEN GAMEOVER = 1: GOTO 3000

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
1610 ATEFOOD = 0
1620 IF NEWX = FOODX AND NEWY = FOODY THEN BEGIN
1630   ATEFOOD = 1
1640   SCORE = SCORE + 10
1650   IF SLEN < MAXLEN THEN SLEN = SLEN + 1
1660   GOSUB 5000  : REM Place new food
1670 BEND

1700 REM === MOVE SNAKE ===
1710 REM Erase tail (unless we ate food)
1720 IF ATEFOOD = 0 THEN BEGIN
1730   TAILX = SX(SLEN - 1)
1740   TAILY = SY(SLEN - 1)
1750   CHAR 0, TAILX, TAILY + 2, " "
1760 BEND
1770 REM Shift body segments
1780 FOR I = SLEN - 1 TO 1 STEP -1
1790   SX(I) = SX(I - 1)
1800   SY(I) = SY(I - 1)
1810 NEXT I
1820 REM Set new head position
1830 SX(0) = NEWX
1840 SY(0) = NEWY

1900 REM === DRAW HEAD ===
1910 CHAR 0, NEWX, NEWY + 2, "@"

2000 REM === UPDATE SCORE ===
2010 CHAR 0, 1, 1, "SCORE: "
2020 CHAR 0, 8, 1, STR$(SCORE)

2100 REM === DELAY FOR GAME SPEED ===
2110 SLEEP SPEED

2200 REM === LOOP ===
2210 GOTO 1030

3000 REM =============================================
3010 REM  GAME OVER
3020 REM =============================================
3030 CHAR 0, 10, 10, "*** GAME OVER ***"
3040 CHAR 0, 10, 12, "Final Score: "
3050 CHAR 0, 23, 12, STR$(SCORE)
3060 CHAR 0, 10, 14, "Press any key..."
3070 GETKEY K$
3080 SCNCLR
3090 PRINT "Thanks for playing SNAKE!"
3100 PRINT "Final score: "; SCORE
3110 PRINT
3120 INPUT "Play again? (Y/N): "; PA$
3130 IF PA$ = "Y" OR PA$ = "y" THEN RUN
3140 END

4000 REM =============================================
4010 REM  DRAW GAME FIELD
4020 REM =============================================
4030 SCNCLR
4040 REM Draw score area
4050 CHAR 0, 1, 1, "SCORE: 0"
4060 REM Draw top border
4070 FOR X = 0 TO FIELDW + 1
4080 CHAR 0, X, 2, "#"
4090 NEXT X
4100 REM Draw side borders
4110 FOR Y = 1 TO FIELDH
4120 CHAR 0, 0, Y + 2, "#"
4130 CHAR 0, FIELDW + 1, Y + 2, "#"
4140 NEXT Y
4150 REM Draw bottom border
4160 FOR X = 0 TO FIELDW + 1
4170 CHAR 0, X, FIELDH + 3, "#"
4180 NEXT X
4190 REM Draw initial snake
4200 FOR I = 0 TO SLEN - 1
4210 IF I = 0 THEN CH$ = "@" ELSE CH$ = "O"
4220 CHAR 0, SX(I), SY(I) + 2, CH$
4230 NEXT I
4240 REM Draw food
4250 CHAR 0, FOODX, FOODY + 2, "*"
4260 RETURN

5000 REM =============================================
5010 REM  PLACE FOOD AT RANDOM POSITION
5020 REM =============================================
5030 FOODX = INT(RND(1) * FIELDW) + 1
5040 FOODY = INT(RND(1) * FIELDH) + 1
5050 REM Check not on snake
5060 FOR I = 0 TO SLEN - 1
5070 IF FOODX = SX(I) AND FOODY = SY(I) THEN GOTO 5030
5080 NEXT I
5090 REM Draw food
5100 CHAR 0, FOODX, FOODY + 2, "*"
5110 RETURN
