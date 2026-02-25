10 REM =============================================
20 REM  BEGIN/BEND Block Test
30 REM  Tests multi-line IF blocks
40 REM =============================================
50 PRINT CHR$(147)
60 PRINT "=== BEGIN/BEND BLOCK TEST ==="
70 PRINT
80 TP% = 0 : TF% = 0
90 REM
100 REM === TEST 1: THEN BEGIN with true condition ===
110 PRINT "TEST 1: IF TRUE THEN BEGIN...BEND"
120 X = 10
130 IF X > 5 THEN BEGIN
140 PRINT "  Inside BEGIN block (X > 5)"
150 TP% = TP% + 1
160 BEND
170 PRINT
180 REM
200 REM === TEST 2: THEN BEGIN with false condition ===
210 PRINT "TEST 2: IF FALSE THEN BEGIN...BEND"
220 Y = 3
230 IF Y > 100 THEN BEGIN
240 PRINT "  [FAIL] This should NOT print!"
250 TF% = TF% + 1
260 BEND
270 TP% = TP% + 1
280 PRINT "  Skipped block correctly"
290 PRINT
300 REM
400 REM === TEST 3: Multiple statements in block ===
410 PRINT "TEST 3: Multiple statements in block"
420 A = 42
430 IF A = 42 THEN BEGIN
440 PRINT "  Statement 1: A = "; A
450 B = A * 2
460 PRINT "  Statement 2: B = "; B
470 C$ = "hello"
480 PRINT "  Statement 3: C$ = "; C$
490 TP% = TP% + 1
500 BEND
510 PRINT
520 REM
600 REM === TEST 4: Single-line IF still works ===
610 PRINT "TEST 4: Single-line IF (no BEGIN)"
620 IF 1 = 1 THEN PRINT "  Single-line IF works"
630 IF 1 = 0 THEN PRINT "  [FAIL] Should not print"
640 TP% = TP% + 1
650 PRINT
660 REM
700 REM === TEST 5: ELSE with BEGIN/BEND (same line) ===
710 PRINT "TEST 5: BEND : ELSE BEGIN (same line)"
720 Z = 3
730 IF Z > 10 THEN BEGIN
740 PRINT "  [FAIL] Z > 10 block should not execute"
750 TF% = TF% + 1
760 BEND : ELSE BEGIN
770 PRINT "  ELSE block executed correctly (Z <= 10)"
780 TP% = TP% + 1
790 BEND
800 PRINT
810 REM
850 REM === TEST 6: ELSE on separate line from BEND ===
860 PRINT "TEST 6: BEND on one line, ELSE on next"
870 W = 7
880 IF W > 100 THEN BEGIN
890 PRINT "  [FAIL] W > 100 block should not execute"
900 TF% = TF% + 1
910 BEND
920 ELSE BEGIN
930 PRINT "  ELSE on separate line works (W <= 100)"
940 TP% = TP% + 1
950 BEND
960 PRINT
970 REM
1000 REM === RESULTS ===
1010 PRINT "==============================="
1020 PRINT "Tests passed: "; TP%
1030 PRINT "Tests failed: "; TF%
1040 PRINT
1050 IF TF% = 0 THEN PRINT "ALL TESTS PASSED!"
1060 IF TF% > 0 THEN PRINT "SOME TESTS FAILED!"
1070 PRINT
1080 END
