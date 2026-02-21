10 REM =============================================
20 REM  SedaiBasic PEEK/POKE Test Suite
30 REM  Tests C128 Memory Mapping Compatibility
40 REM  Run with sbv.exe for full graphics support
50 REM =============================================
60 PRINT CHR$(147)
70 PRINT "=== PEEK/POKE C128 COMPATIBILITY TEST ==="
80 PRINT
90 TP% = 0 : TF% = 0

100 REM =============================================
110 REM  VIC-II ADDRESSES (Reference)
120 REM =============================================
130 REM 53280 = Border color
140 REM 53281 = Background color 0
150 REM 53282-53284 = Background colors 1-3
160 REM 53285-53286 = Sprite multicolors
170 REM 53287-53294 = Sprite colors 0-7
180 REM 53296 = Fast mode (C128)

200 REM =============================================
210 REM  TEST 1: Border Color (53280)
220 REM =============================================
230 PRINT "TEST 1: Border Color (POKE 53280)"
240 PRINT "------------------------------"
250 OB = PEEK(53280)
260 PRINT "Original border: "; OB
270 FOR C = 0 TO 15
280 POKE 53280, C
290 SLEEP 0.2
300 NEXT C
310 POKE 53280, OB
320 NB = PEEK(53280)
330 TN$ = "Border restore": EX = OB: AC = NB: GOSUB 9500
340 PRINT

400 REM =============================================
410 REM  TEST 2: Background Color (53281)
420 REM =============================================
430 PRINT "TEST 2: Background Color (POKE 53281)"
440 PRINT "------------------------------"
450 OBG = PEEK(53281)
460 PRINT "Original background: "; OBG
470 POKE 53281, 0
480 SLEEP 0.5
490 POKE 53281, 6
500 SLEEP 0.5
510 POKE 53281, OBG
520 NBG = PEEK(53281)
530 TN$ = "Background restore": EX = OBG: AC = NBG: GOSUB 9500
540 PRINT

600 REM =============================================
610 REM  TEST 3: Cursor Position (211, 214)
620 REM =============================================
630 PRINT "TEST 3: Cursor Position (211=col, 214=row)"
640 PRINT "------------------------------"
650 OC = PEEK(211)
660 OR = PEEK(214)
670 PRINT "Cursor at col "; OC; ", row "; OR
680 POKE 211, 20
690 POKE 214, 10
700 NC = PEEK(211)
710 NR = PEEK(214)
720 PRINT "After POKE: col "; NC; ", row "; NR
730 POKE 211, OC
740 POKE 214, OR
750 TN$ = "Cursor col POKE": EX = 20: AC = NC: GOSUB 9500
760 TN$ = "Cursor row POKE": EX = 10: AC = NR: GOSUB 9500
770 PRINT

800 REM =============================================
810 REM  TEST 4: Screen Line Length (216)
820 REM =============================================
830 PRINT "TEST 4: Screen Line Length (PEEK 216)"
840 PRINT "------------------------------"
850 SL = PEEK(216)
860 PRINT "Screen width: "; SL; " columns"
870 IF SL = 40 OR SL = 80 THEN TP% = TP% + 1: PRINT "  [PASS] Valid width"
880 IF SL <> 40 AND SL <> 80 THEN TF% = TF% + 1: PRINT "  [FAIL] Invalid width: "; SL
890 PRINT

1000 REM =============================================
1010 REM  TEST 5: Character Color (646)
1020 REM =============================================
1030 PRINT "TEST 5: Character Color (646)"
1040 PRINT "------------------------------"
1050 OCC = PEEK(646)
1060 PRINT "Original char color: "; OCC
1070 POKE 646, 2
1080 PRINT "This text should be RED"
1090 POKE 646, 5
1100 PRINT "This text should be GREEN"
1110 POKE 646, 7
1120 PRINT "This text should be YELLOW"
1130 POKE 646, OCC
1140 PRINT "Back to original color"
1150 TP% = TP% + 1
1160 PRINT

1200 REM =============================================
1210 REM  TEST 6: Screen Memory (1024-2023)
1220 REM =============================================
1230 PRINT "TEST 6: Screen Memory Direct Access"
1240 PRINT "------------------------------"
1250 REM Read character at position (0,0)
1260 SC = PEEK(1024)
1270 PRINT "Char at (0,0): "; SC; " = '"; CHR$(SC); "'"
1280 REM Write a character
1290 POKE 1064, 1  : REM 'A' at position 40 (row 1, col 0)
1300 RC = PEEK(1064)
1310 TN$ = "Screen memory write": EX = 1: AC = RC: GOSUB 9500
1320 POKE 1064, 32 : REM Clear it
1330 PRINT

1400 REM =============================================
1410 REM  TEST 7: Color RAM (55296-56295)
1420 REM =============================================
1430 PRINT "TEST 7: Color RAM Direct Access"
1440 PRINT "------------------------------"
1450 REM Read color at position (0,0)
1460 CC = PEEK(55296)
1470 PRINT "Color at (0,0): "; CC
1480 REM Write a color at position 40 (row 1, col 0)
1490 POKE 55336, 2  : REM Red color
1500 RCC = PEEK(55336)
1510 TN$ = "Color RAM write": EX = 2: AC = RCC: GOSUB 9500
1520 PRINT

1600 REM =============================================
1610 REM  TEST 8: SID Volume (54296)
1620 REM =============================================
1630 PRINT "TEST 8: SID Volume (54296)"
1640 PRINT "------------------------------"
1650 OV = PEEK(54296)
1660 PRINT "Original volume: "; OV
1670 POKE 54296, 8
1680 NV = PEEK(54296)
1690 TN$ = "SID Volume POKE": EX = 8: AC = NV: GOSUB 9500
1700 POKE 54296, OV
1710 PRINT

1800 REM =============================================
1810 REM  TEST 9: Fast Mode (53296)
1820 REM =============================================
1830 PRINT "TEST 9: Fast Mode (53296) - C128 Only"
1840 PRINT "------------------------------"
1850 FM = PEEK(53296)
1860 PRINT "Fast mode: "; FM
1870 IF FM = 0 OR FM = 1 THEN TP% = TP% + 1: PRINT "  [PASS] Valid fast mode value"
1880 IF FM <> 0 AND FM <> 1 THEN TF% = TF% + 1: PRINT "  [FAIL] Invalid fast mode"
1890 PRINT

2000 REM =============================================
2010 REM  TEST 10: Sprite Enable (53269)
2020 REM =============================================
2030 PRINT "TEST 10: Sprite Enable (53269)"
2040 PRINT "------------------------------"
2050 SE = PEEK(53269)
2060 PRINT "Sprite enable bitmap: "; SE
2070 REM Enable sprite 0 (bit 0)
2080 POKE 53269, 1
2090 SE2 = PEEK(53269)
2100 TN$ = "Sprite enable POKE": EX = 1: AC = SE2: GOSUB 9500
2110 REM Disable all sprites
2120 POKE 53269, 0
2130 PRINT

2200 REM =============================================
2210 REM  TEST 11: Sprite Position (53248-53263)
2220 REM =============================================
2230 PRINT "TEST 11: Sprite Position Registers"
2240 PRINT "------------------------------"
2250 REM Sprite 0: X at 53248, Y at 53249
2260 POKE 53248, 100 : REM X low byte
2270 POKE 53249, 100 : REM Y position
2280 SX = PEEK(53248)
2290 SY = PEEK(53249)
2300 PRINT "Sprite 0 position: "; SX; ","; SY
2310 TN$ = "Sprite X pos": EX = 100: AC = SX: GOSUB 9500
2320 TN$ = "Sprite Y pos": EX = 100: AC = SY: GOSUB 9500
2330 PRINT

2400 REM =============================================
2410 REM  TEST 12: Sprite Color (53287)
2420 REM =============================================
2430 PRINT "TEST 12: Sprite Color (53287)"
2440 PRINT "------------------------------"
2450 POKE 53287, 7  : REM Sprite 0 color = yellow
2460 SC = PEEK(53287)
2470 TN$ = "Sprite 0 color": EX = 7: AC = SC: GOSUB 9500
2480 PRINT

2600 REM =============================================
2610 REM  TEST 13: Sprite Collision (53278-53279)
2620 REM =============================================
2630 PRINT "TEST 13: Collision Registers (Read-only)"
2640 PRINT "------------------------------"
2650 SSC = PEEK(53278)  : REM Sprite-sprite collision
2660 SBC = PEEK(53279)  : REM Sprite-background collision
2670 PRINT "Sprite-sprite collision: "; SSC
2680 PRINT "Sprite-background collision: "; SBC
2690 TP% = TP% + 1
2700 PRINT

2800 REM =============================================
2810 REM  TEST 14: Unmapped Address
2820 REM =============================================
2830 PRINT "TEST 14: Unmapped Address (should return 0)"
2840 PRINT "------------------------------"
2850 UA = PEEK(12345)
2860 PRINT "PEEK(12345) = "; UA
2870 TN$ = "Unmapped returns 0": EX = 0: AC = UA: GOSUB 9500
2880 PRINT

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
3130 PRINT "C128 Memory Map Summary:"
3140 PRINT "  VIC-II: 53248-53296"
3150 PRINT "  SID: 54272-54296"
3160 PRINT "  Screen: 1024-2023"
3170 PRINT "  Color RAM: 55296-56295"
3180 PRINT "  Zero Page: 198,199,208,211,212,214,216,646"
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
