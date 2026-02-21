10 REM =============================================
20 REM  SedaiBasic Sprite Test Suite
30 REM  Tests: SPRITE, MOVSPR, SPRCOLOR, COLLISION, BUMP
40 REM  Run with sbv.exe (requires SDL2 graphics)
50 REM =============================================

100 REM === INITIALIZATION ===
110 GRAPHIC 1  : REM Bitmap mode
120 SCNCLR
130 PRINT "=== SPRITE TEST SUITE ==="
140 PRINT
150 TP% = 0 : TF% = 0

200 REM =============================================
210 REM  TEST 1: Basic Sprite Enable/Disable
220 REM =============================================
230 PRINT "TEST 1: SPRITE Enable/Disable"
240 PRINT "------------------------------"
250 REM Enable sprite 1 with color 2 (red)
260 SPRITE 1, 1, 2
270 E1 = RSPRITE(1, 0)  : REM Get enabled status
280 TN$ = "Sprite 1 enabled": EX = 1: AC = E1: GOSUB 9500
290 REM Disable sprite 1
300 SPRITE 1, 0, 2
310 E2 = RSPRITE(1, 0)
320 TN$ = "Sprite 1 disabled": EX = 0: AC = E2: GOSUB 9500
330 PRINT

400 REM =============================================
410 REM  TEST 2: Sprite Color
420 REM =============================================
430 PRINT "TEST 2: Sprite Color"
440 PRINT "------------------------------"
450 SPRITE 1, 1, 5  : REM Green
460 C1 = RSPRITE(1, 1)
470 TN$ = "Sprite 1 color": EX = 5: AC = C1: GOSUB 9500
480 SPRITE 2, 1, 7  : REM Yellow
490 C2 = RSPRITE(2, 1)
500 TN$ = "Sprite 2 color": EX = 7: AC = C2: GOSUB 9500
510 PRINT

600 REM =============================================
610 REM  TEST 3: Sprite Priority
620 REM =============================================
630 PRINT "TEST 3: Sprite Priority"
640 PRINT "------------------------------"
650 SPRITE 1, 1, 2, 0  : REM Priority 0 (background)
660 P1 = RSPRITE(1, 2)
670 TN$ = "Priority 0": EX = 0: AC = P1: GOSUB 9500
680 SPRITE 1, 1, 2, 3  : REM Priority 3 (foreground)
690 P2 = RSPRITE(1, 2)
700 TN$ = "Priority 3": EX = 3: AC = P2: GOSUB 9500
710 PRINT

800 REM =============================================
810 REM  TEST 4: Sprite Scaling
820 REM =============================================
830 PRINT "TEST 4: Sprite Scale"
840 PRINT "------------------------------"
850 SPRITE 1, 1, 2, 0, 2, 2  : REM Double size
860 SX = RSPRITE(1, 3)  : REM Scale X
870 SY = RSPRITE(1, 4)  : REM Scale Y
880 PRINT "Scale X: "; SX
890 PRINT "Scale Y: "; SY
900 IF SX >= 1.9 AND SX <= 2.1 THEN TP% = TP% + 1: PRINT "  [PASS] Scale X"
910 IF SX < 1.9 OR SX > 2.1 THEN TF% = TF% + 1: PRINT "  [FAIL] Scale X"
920 IF SY >= 1.9 AND SY <= 2.1 THEN TP% = TP% + 1: PRINT "  [PASS] Scale Y"
930 IF SY < 1.9 OR SY > 2.1 THEN TF% = TF% + 1: PRINT "  [FAIL] Scale Y"
940 SPRITE 1, 1, 2, 0, 1, 1  : REM Reset to normal
950 PRINT

1000 REM =============================================
1010 REM  TEST 5: MOVSPR Absolute Position
1020 REM =============================================
1030 PRINT "TEST 5: MOVSPR Absolute"
1040 PRINT "------------------------------"
1050 SPRITE 1, 1, 2
1060 MOVSPR 1, 100, 80
1070 X1 = RSPPOS(1, 0)  : REM X position
1080 Y1 = RSPPOS(1, 1)  : REM Y position
1090 PRINT "Position: "; X1; ","; Y1
1100 TN$ = "MOVSPR X": EX = 100: AC = X1: GOSUB 9500
1110 TN$ = "MOVSPR Y": EX = 80: AC = Y1: GOSUB 9500
1120 PRINT

1200 REM =============================================
1210 REM  TEST 6: MOVSPR Relative Movement
1220 REM =============================================
1230 PRINT "TEST 6: MOVSPR Relative (+)"
1240 PRINT "------------------------------"
1250 MOVSPR 1, 100, 80  : REM Start position
1260 MOVSPR 1, +20, +10  : REM Relative move
1270 X2 = RSPPOS(1, 0)
1280 Y2 = RSPPOS(1, 1)
1290 PRINT "After +20,+10: "; X2; ","; Y2
1300 TN$ = "Relative X": EX = 120: AC = X2: GOSUB 9500
1310 TN$ = "Relative Y": EX = 90: AC = Y2: GOSUB 9500
1320 PRINT

1400 REM =============================================
1410 REM  TEST 7: SPRCOLOR (Multicolors)
1420 REM =============================================
1430 PRINT "TEST 7: SPRCOLOR Multicolors"
1440 PRINT "------------------------------"
1450 SPRCOLOR 1, 2  : REM MC1=white, MC2=red
1460 MC1 = RSPCOLOR(1)
1470 MC2 = RSPCOLOR(2)
1480 TN$ = "Multicolor 1": EX = 1: AC = MC1: GOSUB 9500
1490 TN$ = "Multicolor 2": EX = 2: AC = MC2: GOSUB 9500
1500 PRINT

1600 REM =============================================
1610 REM  TEST 8: Multiple Sprites
1620 REM =============================================
1630 PRINT "TEST 8: Multiple Sprites"
1640 PRINT "------------------------------"
1650 REM Enable 4 sprites at different positions
1660 SPRITE 1, 1, 2: MOVSPR 1, 50, 50
1670 SPRITE 2, 1, 5: MOVSPR 2, 100, 50
1680 SPRITE 3, 1, 7: MOVSPR 3, 150, 50
1690 SPRITE 4, 1, 3: MOVSPR 4, 200, 50
1700 REM Verify all enabled
1710 E = 0
1720 IF RSPRITE(1, 0) = 1 THEN E = E + 1
1730 IF RSPRITE(2, 0) = 1 THEN E = E + 1
1740 IF RSPRITE(3, 0) = 1 THEN E = E + 1
1750 IF RSPRITE(4, 0) = 1 THEN E = E + 1
1760 TN$ = "4 sprites enabled": EX = 4: AC = E: GOSUB 9500
1770 PRINT

1800 REM =============================================
1810 REM  TEST 9: BUMP (Collision Status)
1820 REM =============================================
1830 PRINT "TEST 9: BUMP Collision Query"
1840 PRINT "------------------------------"
1850 REM Read collision registers
1860 SS = BUMP(1)  : REM Sprite-sprite collision
1870 SB = BUMP(2)  : REM Sprite-background collision
1880 PRINT "Sprite-sprite collision: "; SS
1890 PRINT "Sprite-background collision: "; SB
1900 TP% = TP% + 1  : REM Just verify no crash
1910 PRINT "  [PASS] BUMP function works"
1920 PRINT

2000 REM =============================================
2010 REM  TEST 10: SPRSAV (Save/Load sprite data)
2020 REM =============================================
2030 PRINT "TEST 10: SPRSAV"
2040 PRINT "------------------------------"
2050 REM Save sprite 1 data to string
2060 SPRSAV 1, SP1$
2070 PRINT "Sprite data saved, length: "; LEN(SP1$)
2080 IF LEN(SP1$) > 0 THEN TP% = TP% + 1: PRINT "  [PASS] SPRSAV"
2090 IF LEN(SP1$) = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] SPRSAV"
2100 REM Copy to sprite 5
2110 SPRSAV SP1$, 5
2120 PRINT "  Sprite data loaded to sprite 5"
2130 PRINT

2200 REM =============================================
2210 REM  TEST 11: Visual Demo
2220 REM =============================================
2230 PRINT "TEST 11: Visual Demo (watch sprites move)"
2240 PRINT "------------------------------"
2250 PRINT "Press any key to start demo..."
2260 GETKEY K$
2270 REM Disable all sprites first
2280 FOR I = 1 TO 8: SPRITE I, 0, 0: NEXT I
2290 REM Enable 3 sprites
2300 SPRITE 1, 1, 2: MOVSPR 1, 50, 100
2310 SPRITE 2, 1, 5: MOVSPR 2, 150, 100
2320 SPRITE 3, 1, 7: MOVSPR 3, 250, 100
2330 REM Move them around
2340 FOR FRAME = 1 TO 50
2350   MOVSPR 1, +2, 0
2360   MOVSPR 2, 0, +1
2370   MOVSPR 3, -2, 0
2380   SLEEP 0.05
2390 NEXT FRAME
2400 PRINT "  Demo complete!"
2410 TP% = TP% + 1
2420 PRINT

2500 REM === CLEANUP ===
2510 FOR I = 1 TO 8: SPRITE I, 0, 0: NEXT I

3000 REM =============================================
3010 REM  FINAL RESULTS
3020 REM =============================================
3030 PRINT "========================================="
3040 PRINT "         TEST RESULTS SUMMARY"
3050 PRINT "========================================="
3060 PRINT
3070 PRINT "Tests passed: "; TP%
3080 PRINT "Tests failed: "; TF%
3090 PRINT
3100 IF TF% = 0 THEN PRINT "ALL TESTS PASSED!"
3110 IF TF% > 0 THEN PRINT "SOME TESTS FAILED"
3120 PRINT
3130 PRINT "Press any key to exit..."
3140 GETKEY K$
3150 GRAPHIC 0
3160 END

9500 REM *** Assert numeric equality ***
9510 IF EX = AC THEN GOTO 9560
9520 PRINT "  [FAIL] "; TN$; " expected "; EX; " got "; AC
9530 TF% = TF% + 1
9540 RETURN
9560 PRINT "  [PASS] "; TN$
9570 TP% = TP% + 1
9580 RETURN
