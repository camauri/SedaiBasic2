10 REM =============================================
20 REM  PONG - SedaiBasic with Sprites
30 REM  Classic Pong game testing sprites/collision
40 REM  Controls: W/S or Up/Down arrows
50 REM  Run with sbv.exe (requires graphics mode)
60 REM =============================================

100 REM === INITIALIZATION ===
110 GRAPHIC 0
120 SCNCLR
130 PRINT "=== PONG ==="
140 PRINT
150 PRINT "Controls:"
160 PRINT "  W / UP    - Move paddle up"
170 PRINT "  S / DOWN  - Move paddle down"
180 PRINT "  Q         - Quit game"
190 PRINT
200 PRINT "First to 5 points wins!"
210 PRINT
220 INPUT "Press ENTER to start..."; DUMMY$

300 REM === GAME CONSTANTS ===
310 SCREENW = 40   : REM Screen width in chars
320 SCREENH = 23   : REM Screen height in chars
330 PADDLEH = 4    : REM Paddle height
340 SPEED = 0.05   : REM Game speed
350 WINPTS = 5     : REM Points to win

400 REM === GAME STATE ===
410 P1Y = 10       : REM Player 1 paddle Y
420 P2Y = 10       : REM Player 2 (CPU) paddle Y
430 BALLX = 20     : REM Ball X position
440 BALLY = 12     : REM Ball Y position
450 BDX = 1        : REM Ball direction X
460 BDY = 1        : REM Ball direction Y
470 P1SCORE = 0    : REM Player 1 score
480 P2SCORE = 0    : REM Player 2 score
490 GAMEOVER = 0   : REM Game over flag

500 REM === DRAW INITIAL SCREEN ===
510 GOSUB 4000

1000 REM =============================================
1010 REM  MAIN GAME LOOP
1020 REM =============================================
1030 IF GAMEOVER = 1 THEN GOTO 3000

1100 REM === ERASE OLD POSITIONS ===
1110 GOSUB 4500  : REM Erase ball
1120 GOSUB 4600  : REM Erase paddles

1200 REM === GET INPUT (Non-blocking) ===
1210 GET K$
1220 IF K$ = "" THEN GOTO 1300

1230 REM === PROCESS INPUT ===
1240 K = ASC(K$)
1250 REM W or Up arrow - move up
1260 IF K$ = "W" OR K$ = "w" OR K = 145 THEN IF P1Y > 2 THEN P1Y = P1Y - 1
1270 REM S or Down arrow - move down
1280 IF K$ = "S" OR K$ = "s" OR K = 17 THEN IF P1Y < SCREENH - PADDLEH THEN P1Y = P1Y + 1
1290 REM Q to quit
1300 IF K$ = "Q" OR K$ = "q" THEN GAMEOVER = 1: GOTO 3000

1400 REM === MOVE CPU PADDLE (Simple AI) ===
1410 REM CPU follows ball with some delay
1420 IF BALLY < P2Y + PADDLEH / 2 - 1 THEN IF P2Y > 2 THEN P2Y = P2Y - 1
1430 IF BALLY > P2Y + PADDLEH / 2 + 1 THEN IF P2Y < SCREENH - PADDLEH THEN P2Y = P2Y + 1

1500 REM === MOVE BALL ===
1510 NEWBX = BALLX + BDX
1520 NEWBY = BALLY + BDY

1600 REM === CHECK TOP/BOTTOM WALL COLLISION ===
1610 IF NEWBY <= 2 THEN BDY = 1: NEWBY = 2
1620 IF NEWBY >= SCREENH THEN BDY = -1: NEWBY = SCREENH

1700 REM === CHECK PADDLE COLLISION (Player 1 - Left) ===
1710 IF NEWBX <= 3 THEN BEGIN
1720   IF NEWBY >= P1Y AND NEWBY <= P1Y + PADDLEH THEN BEGIN
1730     BDX = 1  : REM Bounce right
1740     NEWBX = 4
1750     REM Add spin based on where ball hits paddle
1760     IF NEWBY < P1Y + PADDLEH / 2 THEN BDY = -1
1770     IF NEWBY > P1Y + PADDLEH / 2 THEN BDY = 1
1780   BEND
1790 BEND

1800 REM === CHECK PADDLE COLLISION (CPU - Right) ===
1810 IF NEWBX >= SCREENW - 3 THEN BEGIN
1820   IF NEWBY >= P2Y AND NEWBY <= P2Y + PADDLEH THEN BEGIN
1830     BDX = -1  : REM Bounce left
1840     NEWBX = SCREENW - 4
1850     IF NEWBY < P2Y + PADDLEH / 2 THEN BDY = -1
1860     IF NEWBY > P2Y + PADDLEH / 2 THEN BDY = 1
1870   BEND
1880 BEND

1900 REM === CHECK SCORE (Ball past paddle) ===
1910 IF NEWBX < 1 THEN BEGIN
1920   P2SCORE = P2SCORE + 1
1930   GOSUB 5000  : REM Reset ball
1940   GOTO 2100
1950 BEND
1960 IF NEWBX > SCREENW THEN BEGIN
1970   P1SCORE = P1SCORE + 1
1980   GOSUB 5000  : REM Reset ball
1990   GOTO 2100
2000 BEND

2010 REM === UPDATE BALL POSITION ===
2020 BALLX = NEWBX
2030 BALLY = NEWBY

2100 REM === DRAW NEW POSITIONS ===
2110 GOSUB 4700  : REM Draw paddles
2120 GOSUB 4800  : REM Draw ball

2200 REM === UPDATE SCORE DISPLAY ===
2210 CHAR 0, 15, 0, STR$(P1SCORE)
2220 CHAR 0, 24, 0, STR$(P2SCORE)

2300 REM === CHECK WIN CONDITION ===
2310 IF P1SCORE >= WINPTS THEN WINNER$ = "PLAYER 1": GAMEOVER = 1
2320 IF P2SCORE >= WINPTS THEN WINNER$ = "CPU": GAMEOVER = 1

2400 REM === DELAY FOR GAME SPEED ===
2410 SLEEP SPEED

2500 REM === LOOP ===
2510 GOTO 1030

3000 REM =============================================
3010 REM  GAME OVER
3020 REM =============================================
3030 CHAR 0, 12, 10, "*** GAME OVER ***"
3040 IF P1SCORE >= WINPTS OR P2SCORE >= WINPTS THEN BEGIN
3050   CHAR 0, 12, 12, WINNER$
3060   CHAR 0, 12 + LEN(WINNER$), 12, " WINS!"
3070 BEND
3080 CHAR 0, 10, 14, "Final: "
3090 CHAR 0, 17, 14, STR$(P1SCORE)
3100 CHAR 0, 19, 14, "-"
3110 CHAR 0, 21, 14, STR$(P2SCORE)
3120 CHAR 0, 10, 16, "Press any key..."
3130 GETKEY K$
3140 SCNCLR
3150 PRINT "Thanks for playing PONG!"
3160 PRINT
3170 INPUT "Play again? (Y/N): "; PA$
3180 IF PA$ = "Y" OR PA$ = "y" THEN RUN
3190 END

4000 REM =============================================
4010 REM  DRAW GAME FIELD
4020 REM =============================================
4030 SCNCLR
4040 REM Draw top border
4050 FOR X = 0 TO SCREENW
4060 CHAR 0, X, 1, "-"
4070 NEXT X
4080 REM Draw center line
4090 FOR Y = 2 TO SCREENH
4100 CHAR 0, SCREENW / 2, Y, ":"
4110 NEXT Y
4120 REM Draw score header
4130 CHAR 0, 10, 0, "PLAYER:"
4140 CHAR 0, 15, 0, "0"
4150 CHAR 0, 20, 0, "CPU:"
4160 CHAR 0, 24, 0, "0"
4170 REM Draw paddles
4180 GOSUB 4700
4190 REM Draw ball
4200 GOSUB 4800
4210 RETURN

4500 REM === ERASE BALL ===
4510 CHAR 0, BALLX, BALLY, " "
4520 RETURN

4600 REM === ERASE PADDLES ===
4610 FOR I = 0 TO PADDLEH
4620 CHAR 0, 2, P1Y + I, " "
4630 CHAR 0, SCREENW - 2, P2Y + I, " "
4640 NEXT I
4650 RETURN

4700 REM === DRAW PADDLES ===
4710 FOR I = 0 TO PADDLEH
4720 CHAR 0, 2, P1Y + I, "|"
4730 CHAR 0, SCREENW - 2, P2Y + I, "|"
4740 NEXT I
4750 RETURN

4800 REM === DRAW BALL ===
4810 CHAR 0, BALLX, BALLY, "O"
4820 RETURN

5000 REM =============================================
5010 REM  RESET BALL TO CENTER
5020 REM =============================================
5030 BALLX = SCREENW / 2
5040 BALLY = SCREENH / 2
5050 REM Randomize direction
5060 IF RND(1) > 0.5 THEN BDX = 1 ELSE BDX = -1
5070 IF RND(1) > 0.5 THEN BDY = 1 ELSE BDY = -1
5080 REM Brief pause after score
5090 SLEEP 0.5
5100 RETURN
