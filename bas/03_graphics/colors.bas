10 REM =============================================
20 REM  SedaiBasic Color/Palette Test Suite
30 REM  Tests: COLOR, SETCOLOR, GETCOLOR, PLOAD, PSAVE
40 REM  Run with sbv.exe (requires SDL2 graphics)
50 REM =============================================

100 REM === INITIALIZATION ===
110 GRAPHIC 1  : REM Bitmap mode
120 SCNCLR
130 PRINT "=== COLOR/PALETTE TEST SUITE ==="
140 PRINT
150 TP% = 0 : TF% = 0

200 REM =============================================
210 REM  TEST 1: COLOR Command (Source/Index)
220 REM =============================================
230 PRINT "TEST 1: COLOR Command"
240 PRINT "------------------------------"
250 REM COLOR source, index - Sets color source
260 COLOR 0, 6   : REM Background = blue
270 COLOR 1, 14  : REM Foreground = light blue
280 C0 = RCLR(0) : REM Read back
290 C1 = RCLR(1)
300 TN$ = "Background color": EX = 6: AC = C0: GOSUB 9500
310 TN$ = "Foreground color": EX = 14: AC = C1: GOSUB 9500
320 PRINT

400 REM =============================================
410 REM  TEST 2: RCLR Function
420 REM =============================================
430 PRINT "TEST 2: RCLR (Read Color Source)"
440 PRINT "------------------------------"
450 COLOR 2, 2   : REM Multicolor 1 = red
460 COLOR 3, 5   : REM Multicolor 2 = green
470 COLOR 4, 0   : REM Border = black
480 R2 = RCLR(2)
490 R3 = RCLR(3)
500 R4 = RCLR(4)
510 TN$ = "Multicolor 1": EX = 2: AC = R2: GOSUB 9500
520 TN$ = "Multicolor 2": EX = 5: AC = R3: GOSUB 9500
530 TN$ = "Border color": EX = 0: AC = R4: GOSUB 9500
540 PRINT

600 REM =============================================
610 REM  TEST 3: SETCOLOR (Modify Palette)
620 REM =============================================
630 PRINT "TEST 3: SETCOLOR (Palette Modification)"
640 PRINT "------------------------------"
650 REM Save original color 0
660 OR = GETCOLOR(0, 0)  : REM Red
670 OG = GETCOLOR(0, 1)  : REM Green
680 OB = GETCOLOR(0, 2)  : REM Blue
690 OA = GETCOLOR(0, 3)  : REM Alpha
700 PRINT "Original color 0: R=";OR;" G=";OG;" B=";OB;" A=";OA
710 REM Modify palette entry 0
720 SETCOLOR 0, 0, 255    : REM Red component
730 SETCOLOR 0, 1, 128    : REM Green component
740 SETCOLOR 0, 2, 64     : REM Blue component
750 SETCOLOR 0, 3, 255    : REM Alpha component
760 REM Read back
770 NR = GETCOLOR(0, 0)
780 NG = GETCOLOR(0, 1)
790 NB = GETCOLOR(0, 2)
800 NA = GETCOLOR(0, 3)
810 PRINT "Modified color 0: R=";NR;" G=";NG;" B=";NB;" A=";NA
820 TN$ = "SETCOLOR Red": EX = 255: AC = NR: GOSUB 9500
830 TN$ = "SETCOLOR Green": EX = 128: AC = NG: GOSUB 9500
840 TN$ = "SETCOLOR Blue": EX = 64: AC = NB: GOSUB 9500
850 REM Restore original
860 SETCOLOR 0, 0, OR
870 SETCOLOR 0, 1, OG
880 SETCOLOR 0, 2, OB
890 SETCOLOR 0, 3, OA
900 PRINT

1000 REM =============================================
1010 REM  TEST 4: GETCOLOR Function
1020 REM =============================================
1030 PRINT "TEST 4: GETCOLOR (Read Palette)"
1040 PRINT "------------------------------"
1050 REM Read standard C64 colors
1060 PRINT "C64 Standard Palette:"
1070 FOR I = 0 TO 15
1080   R = GETCOLOR(I, 0)
1090   G = GETCOLOR(I, 1)
1100   B = GETCOLOR(I, 2)
1110   PRINT "  Color "; I; ": R=";R;" G=";G;" B=";B
1120 NEXT I
1130 TP% = TP% + 1
1140 PRINT

1200 REM =============================================
1210 REM  TEST 5: PSAVE/PLOAD (Palette Files)
1220 REM =============================================
1230 PRINT "TEST 5: PSAVE/PLOAD (Palette Files)"
1240 PRINT "------------------------------"
1250 REM Modify a color
1260 SETCOLOR 15, 0, 100
1270 SETCOLOR 15, 1, 150
1280 SETCOLOR 15, 2, 200
1290 REM Save palette
1300 PSAVE "test_palette.json"
1310 PRINT "Palette saved to test_palette.json"
1320 REM Modify color again
1330 SETCOLOR 15, 0, 0
1340 SETCOLOR 15, 1, 0
1350 SETCOLOR 15, 2, 0
1360 REM Load palette back
1370 PLOAD "test_palette.json"
1380 PRINT "Palette loaded from test_palette.json"
1390 REM Verify
1400 LR = GETCOLOR(15, 0)
1410 LG = GETCOLOR(15, 1)
1420 LB = GETCOLOR(15, 2)
1430 PRINT "Color 15 after load: R=";LR;" G=";LG;" B=";LB
1440 TN$ = "PLOAD Red": EX = 100: AC = LR: GOSUB 9500
1450 TN$ = "PLOAD Green": EX = 150: AC = LG: GOSUB 9500
1460 TN$ = "PLOAD Blue": EX = 200: AC = LB: GOSUB 9500
1470 REM Cleanup
1480 SCRATCH "test_palette.json"
1490 PRINT

1600 REM =============================================
1610 REM  TEST 6: Color Cycling Demo
1620 REM =============================================
1630 PRINT "TEST 6: Color Cycling Demo"
1640 PRINT "------------------------------"
1650 PRINT "Press any key to see color cycling..."
1660 GETKEY K$
1670 REM Draw some colored boxes
1680 FOR C = 1 TO 15
1690   COLOR 1, C
1700   BOX 1, C*15, 150, C*15+10, 180, 0, 1
1710 NEXT C
1720 REM Cycle border color
1730 FOR I = 1 TO 30
1740   COLOR 4, I MOD 16
1750   SLEEP 0.1
1760 NEXT I
1770 TP% = TP% + 1
1780 PRINT "  Color cycling complete!"
1790 PRINT

2000 REM =============================================
2010 REM  TEST 7: Visual Color Test
2020 REM =============================================
2030 PRINT "TEST 7: Visual Color Palette"
2040 PRINT "------------------------------"
2050 PRINT "Drawing 16-color palette grid..."
2060 REM Draw color boxes
2070 FOR C = 0 TO 15
2080   X = (C MOD 4) * 60 + 40
2090   Y = INT(C / 4) * 30 + 50
2100   COLOR 1, C
2110   BOX 1, X, Y, X+50, Y+20, 0, 1
2120 NEXT C
2130 TP% = TP% + 1
2140 SLEEP 2
2150 PRINT

3000 REM =============================================
3010 REM  FINAL RESULTS
3020 REM =============================================
3030 SCNCLR
3040 PRINT "========================================="
3050 PRINT "         TEST RESULTS SUMMARY"
3060 PRINT "========================================="
3070 PRINT
3080 PRINT "Tests passed: "; TP%
3090 PRINT "Tests failed: "; TF%
3100 PRINT
3110 IF TF% = 0 THEN PRINT "ALL TESTS PASSED!"
3120 IF TF% > 0 THEN PRINT "SOME TESTS FAILED"
3130 PRINT
3140 PRINT "Press any key to exit..."
3150 GETKEY K$
3160 GRAPHIC 0
3170 END

9500 REM *** Assert numeric equality ***
9510 IF EX = AC THEN GOTO 9560
9520 PRINT "  [FAIL] "; TN$; " expected "; EX; " got "; AC
9530 TF% = TF% + 1
9540 RETURN
9560 PRINT "  [PASS] "; TN$
9570 TP% = TP% + 1
9580 RETURN
