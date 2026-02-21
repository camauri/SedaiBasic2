10 REM =============================================
20 REM  GUESS THE NUMBER - SedaiBasic
30 REM  Classic number guessing game
40 REM  Works with sb.exe (console) or sbv.exe
50 REM =============================================

100 REM === TITLE SCREEN ===
110 PRINT CHR$(147)
120 PRINT "================================"
130 PRINT "     GUESS THE NUMBER GAME"
140 PRINT "================================"
150 PRINT
160 PRINT "I'm thinking of a number between"
170 PRINT "1 and 100. Can you guess it?"
180 PRINT
190 PRINT "After each guess, I'll tell you"
200 PRINT "if your guess is too HIGH or LOW."
210 PRINT
220 PRINT "Try to guess in as few tries as"
230 PRINT "possible!"
240 PRINT
250 INPUT "Press ENTER to start..."; D$

300 REM === GAME VARIABLES ===
310 BESTSCORE = 999
320 GAMES = 0

400 REM =============================================
410 REM  NEW GAME
420 REM =============================================
430 GAMES = GAMES + 1
440 PRINT
450 PRINT "================================"
460 PRINT "         GAME #"; GAMES
470 PRINT "================================"
480 PRINT

500 REM === GENERATE RANDOM NUMBER ===
510 SECRET = INT(RND(1) * 100) + 1
520 TRIES = 0
530 GUESSED = 0

600 REM =============================================
610 REM  GAME LOOP
620 REM =============================================
630 IF GUESSED = 1 THEN GOTO 800

640 REM === GET PLAYER GUESS ===
650 INPUT "Your guess (1-100): "; GUESS
660 TRIES = TRIES + 1

670 REM === VALIDATE INPUT ===
680 IF GUESS < 1 OR GUESS > 100 THEN BEGIN
690   PRINT "Please enter a number between 1 and 100!"
700   TRIES = TRIES - 1
710   GOTO 630
720 BEND

730 REM === CHECK GUESS ===
740 IF GUESS = SECRET THEN GUESSED = 1: GOTO 800
750 IF GUESS < SECRET THEN PRINT "Too LOW! Try higher."
760 IF GUESS > SECRET THEN PRINT "Too HIGH! Try lower."
770 PRINT "Tries so far: "; TRIES
780 PRINT
790 GOTO 630

800 REM =============================================
810 REM  PLAYER WON
820 REM =============================================
830 PRINT
840 PRINT "********************************"
850 PRINT "   CONGRATULATIONS!"
860 PRINT "********************************"
870 PRINT
880 PRINT "You guessed it! The number was "; SECRET
890 PRINT "You got it in "; TRIES; " tries!"
900 PRINT

910 REM === CHECK BEST SCORE ===
920 IF TRIES < BESTSCORE THEN BEGIN
930   BESTSCORE = TRIES
940   PRINT "*** NEW BEST SCORE! ***"
950 BEND
960 PRINT "Your best score: "; BESTSCORE; " tries"
970 PRINT

980 REM === RATE PERFORMANCE ===
990 IF TRIES <= 5 THEN PRINT "EXCELLENT! You're a mind reader!"
1000 IF TRIES > 5 AND TRIES <= 7 THEN PRINT "GREAT JOB! Very efficient!"
1010 IF TRIES > 7 AND TRIES <= 10 THEN PRINT "GOOD! That's reasonable."
1020 IF TRIES > 10 THEN PRINT "Keep practicing! Optimal is 7 or less."
1030 PRINT

1100 REM =============================================
1110 REM  PLAY AGAIN?
1120 REM =============================================
1130 INPUT "Play again? (Y/N): "; PA$
1140 IF PA$ = "Y" OR PA$ = "y" THEN GOTO 400

1200 REM =============================================
1210 REM  FINAL STATISTICS
1220 REM =============================================
1230 PRINT
1240 PRINT "================================"
1250 PRINT "       FINAL STATISTICS"
1260 PRINT "================================"
1270 PRINT
1280 PRINT "Games played: "; GAMES
1290 PRINT "Best score:   "; BESTSCORE; " tries"
1300 PRINT
1310 PRINT "Thanks for playing!"
1320 PRINT
1330 END
