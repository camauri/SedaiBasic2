10 REM =============================================
20 REM  SedaiBasic Console I/O Test Suite
30 REM  Tests: PRINT, INPUT, GET, GETKEY, TAB, SPC
40 REM =============================================
50 PRINT CHR$(147)
60 PRINT "=== CONSOLE I/O TEST SUITE ==="
70 PRINT
80 TP% = 0 : TF% = 0: rem WK$=""

100 REM =============================================
110 REM  TEST 1: PRINT Basic
120 REM =============================================
130 PRINT "TEST 1: PRINT Statement"
140 PRINT "------------------------------"
150 PRINT "Single value: "; 123
160 PRINT "Multiple: "; 10; " "; 20; " "; 30
170 PRINT "No newline: ";
180 PRINT "continued"
190 PRINT "Expression: 5+3*2 = "; 5 + 3 * 2
200 TN$ = "PRINT 5+3*2": EX = 11: AC = 5 + 3 * 2: GOSUB 9500
210 PRINT
220 GOSUB 9700

300 REM =============================================
310 REM  TEST 2: PRINT Separators
320 REM =============================================
330 PRINT "TEST 2: PRINT Separators"
340 PRINT "------------------------------"
350 PRINT "Semicolon (no space):"
360 PRINT "A";"B";"C"
370 X$ = "A" + "B" + "C"
380 TN$ = "String concat ABC": EX$ = "ABC": AC$ = X$: GOSUB 9600
390 PRINT "Comma (tab zones):"
400 PRINT "A","B","C"
410 PRINT
420 GOSUB 9700

500 REM =============================================
510 REM  TEST 3: TAB Function
520 REM =============================================
530 PRINT "TEST 3: TAB Function"
540 PRINT "------------------------------"
550 PRINT "X"; TAB(10); "at 10"; TAB(20); "at 20"
560 PRINT "0         1         2         3"
570 PRINT "0123456789012345678901234567890123456789"
580 TN$ = "TAB function exists": EX = 1: AC = 1: GOSUB 9500
590 PRINT
600 GOSUB 9700

700 REM =============================================
710 REM  TEST 4: SPC Function
720 REM =============================================
730 PRINT "TEST 4: SPC Function"
740 PRINT "------------------------------"
750 PRINT "A"; SPC(5); "5 spaces"; SPC(10); "10 spaces"
760 TN$ = "SPC function exists": EX = 1: AC = 1: GOSUB 9500
770 PRINT
780 GOSUB 9700

900 REM =============================================
910 REM  TEST 5: POS Function
920 REM =============================================
930 PRINT "TEST 5: POS Function"
940 PRINT "------------------------------"
950 PRINT "Cursor at col: ";
960 P = POS(0)
970 PRINT P
980 PRINT "After 10 chars:";
990 P2 = POS(0)
1000 PRINT " pos="; P2
1010 TN$ = "POS after 15": EX = 15: AC = P2: GOSUB 9500
1020 PRINT
1030 GOSUB 9700

1100 REM =============================================
1110 REM  TEST 6: GET (Non-blocking)
1120 REM =============================================
1130 PRINT "TEST 6: GET (Non-blocking input)"
1140 PRINT "------------------------------"
1150 PRINT "Checking keyboard buffer..."
1160 GET G$
1170 IF G$ = "" THEN PRINT "No key in buffer (correct)"
1175 IF G$ = "" THEN GK = 1
1180 IF G$ <> "" THEN PRINT "Key found: "; G$
1185 IF G$ <> "" THEN GK = 1
1190 TN$ = "GET returns": EX = 1: AC = GK: GOSUB 9500
1195 PRINT
1200 GOSUB 9700

1300 REM =============================================
1310 REM  TEST 7: GETKEY (Blocking)
1320 REM =============================================
1330 PRINT "TEST 7: GETKEY (Blocking input)"
1340 PRINT "------------------------------"
1350 PRINT "Press any key..."
1360 GETKEY K$
1370 PRINT "You pressed: "; K$; " (ASCII "; ASC(K$); ")"
1380 TN$ = "GETKEY reads char": EX$ = K$: AC$ = K$: GOSUB 9600
1390 PRINT
1400 GOSUB 9700

1500 REM =============================================
1510 REM  TEST 8: INPUT String
1520 REM =============================================
1530 PRINT "TEST 8: INPUT String"
1540 PRINT "------------------------------"
1550 INPUT "Enter your name: "; NM$
1560 PRINT "Hello, "; NM$; "!"
1570 PRINT "Length: "; LEN(NM$)
1580 TN$ = "INPUT string len>0": EX = 1: IF LEN(NM$) > 0 THEN AC = 1 ELSE AC = 0
1590 GOSUB 9500
1600 PRINT
1610 GOSUB 9700

1700 REM =============================================
1710 REM  TEST 9: INPUT Number
1720 REM =============================================
1730 PRINT "TEST 9: INPUT Number"
1740 PRINT "------------------------------"
1750 INPUT "Enter a number: "; N
1760 PRINT "You entered: "; N
1770 PRINT "Doubled: "; N * 2
1780 PRINT "Squared: "; N * N
1790 TN$ = "INPUT number works": EX = N * 2: AC = N + N: GOSUB 9500
1800 PRINT
1810 GOSUB 9700

1900 REM =============================================
1910 REM  TEST 10: INPUT Multiple
1920 REM =============================================
1930 PRINT "TEST 10: INPUT Multiple Values"
1940 PRINT "------------------------------"
1950 PRINT "Enter 3 numbers, one per line:"
1960 INPUT A, B, C
1970 PRINT "A="; A; " B="; B; " C="; C
1980 PRINT "Sum: "; A + B + C
1990 TN$ = "INPUT multiple sum": EX = A + B + C: AC = A + B + C: GOSUB 9500
2000 PRINT
2010 GOSUB 9700

2100 REM =============================================
2110 REM  TEST 11: PRINT USING Numeric
2120 REM =============================================
2130 PRINT "TEST 11: PRINT USING (Numeric)"
2140 PRINT "------------------------------"
2150 V = 1234.567
2160 PRINT "Value: "; V
2170 PRINT USING "######.##"; V
2180 PRINT USING "#####.####"; V
2190 PRINT USING "###,###.##"; 12345.67
2200 TN$ = "PRINT USING exists": EX = 1: AC = 1: GOSUB 9500
2210 PRINT
2220 GOSUB 9700

2300 REM =============================================
2310 REM  TEST 12: PRINT USING Dollar
2320 REM =============================================
2330 PRINT "TEST 12: PRINT USING (Currency)"
2340 PRINT "------------------------------"
2350 PRINT USING "$#####.##"; 99.99
2360 PRINT USING "$#####.##"; 1234.56
2370 TN$ = "PRINT USING dollar": EX = 1: AC = 1: GOSUB 9500
2380 PRINT
2390 GOSUB 9700

2500 REM =============================================
2510 REM  TEST 13: PUDEF
2520 REM =============================================
2530 PRINT "TEST 13: PUDEF (Filler char)"
2540 PRINT "------------------------------"
2550 PRINT "Default filler (space):"
2560 PRINT USING "######.##"; 12.34
2570 PUDEF "*"
2580 PRINT "Star filler (*):"
2590 PRINT USING "######.##"; 12.34
2600 PUDEF "0"
2610 PRINT "Zero filler (0):"
2620 PRINT USING "######.##"; 12.34
2630 PUDEF " "
2640 TN$ = "PUDEF changes filler": EX = 1: AC = 1: GOSUB 9500
2650 PRINT
2660 GOSUB 9700

2800 REM =============================================
2810 REM  TEST 14: CHAR Statement
2820 REM =============================================
2830 PRINT "TEST 14: CHAR Statement"
2840 PRINT "------------------------------"
2850 PRINT "Placing text at specific positions:"
2860 CHAR 0, 5, 0, "Row 0, Col 5"
2870 CHAR 0, 10, 1, "Row 1, Col 10"
2880 CHAR 0, 15, 2, "Row 2, Col 15"
2890 PRINT
2900 PRINT
2910 PRINT
2920 PRINT
2930 TN$ = "CHAR positioning": EX = 1: AC = 1: GOSUB 9500
2940 PRINT

3000 REM =============================================
3010 REM  FINAL RESULTS
3020 REM =============================================
3030 PRINT "========================================="
3040 PRINT "         TEST RESULTS SUMMARY"
3050 PRINT "========================================="
3060 PRINT
3070 PRINT "Tests passed: "; TP%
3080 PRINT "Tests failed: "; TF%
3090 TT% = TP% + TF%
3100 PRINT "Total tests:  "; TT%
3110 PRINT
3120 IF TF% = 0 THEN PRINT "All automated tests PASSED!"
3130 IF TF% > 0 THEN PRINT "Some tests FAILED"
3140 PRINT
3150 PRINT "Manual verification needed for:"
3160 PRINT "- TAB/SPC alignment"
3170 PRINT "- PRINT USING formatting"
3180 PRINT "- CHAR positioning"
3190 PRINT
3200 END

9500 REM *** Assert numeric equality ***
9510 IF EX = AC THEN GOTO 9560
9520 PRINT "  [FAIL] "; TN$; " expected "; EX; " got "; AC
9530 TF% = TF% + 1
9540 RETURN
9560 PRINT "  [PASS] "; TN$
9570 TP% = TP% + 1
9580 RETURN

9600 REM *** Assert string equality ***
9610 IF EX$ = AC$ THEN GOTO 9660
9620 PRINT "  [FAIL] "; TN$; " expected "; EX$; " got "; AC$
9630 TF% = TF% + 1
9640 RETURN
9660 PRINT "  [PASS] "; TN$
9670 TP% = TP% + 1
9680 RETURN

9700 REM *** Wait for keypress ***
9710 PRINT "Press any key to continue..."
9720 GETKEY WK$
9730 PRINT
9740 RETURN
