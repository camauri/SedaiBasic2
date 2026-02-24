10 REM === SedaiBasic Color & Palette Demo ===
20 REM Demonstrates palette capabilities
100 REM ======== Page 1: Introduction ========
110 SCNCLR
120 PRINT "============================================"
130 PRINT "     SedaiBasic Color & Palette Demo"
140 PRINT "============================================"
150 PRINT
160 PRINT "This program demonstrates the color palette"
170 PRINT "capabilities of SedaiBasic."
180 PRINT
190 PRINT "Demo pages:"
200 PRINT "  1. The 16 standard C64 colors"
210 PRINT "  2. Full 256-color default palette"
220 PRINT "  3. Color wheel: 16 hues x 16 brightness"
230 PRINT "  4. Standard VGA 256-color palette"
240 PRINT "  5. Smooth rainbow gradient (256 colors)"
250 PRINT
260 PRINT "Press any key to begin..."
270 GETKEY K$
300 REM ======== Page 2: 16 C64 Colors ========
310 SCNCLR
320 PRINT "Page 1: The 16 Standard C64 Colors"
330 PRINT "-----------------------------------"
340 PRINT "The classic Commodore 64/128 palette"
350 PRINT "displayed as a 4x4 grid of filled boxes."
360 PRINT
370 PRINT "Press any key to view..."
380 GETKEY K$
390 GRAPHIC 1
395 FAST
400 COLOR 0, 12 : SCNCLR : REM dark grey background
405 REM Map sources 0-3 to palette 0-3 for correct BOX colors
406 COLOR 0, 1 : COLOR 1, 2 : COLOR 2, 3 : COLOR 3, 4
410 FOR I = 0 TO 15
420 RW = INT(I / 4) : CL = I - RW * 4
430 X1 = 10 + CL * 75 : Y1 = 10 + RW * 45
440 X2 = X1 + 68 : Y2 = Y1 + 38
450 BOX I, X1, Y1, X2, Y2, 0, 1
460 NEXT I
465 SLOW
470 GETKEY K$
480 GRAPHIC 0
485 GOSUB 8500 : REM restore text colors
500 REM ======== Page 3: 256 Default Palette ========
510 SCNCLR
520 PRINT "Page 2: Full 256-Color Palette (Default)"
530 PRINT "----------------------------------------"
540 PRINT "The default palette has 256 entries:"
550 PRINT "  Entries 0-15:   standard C64 colors"
560 PRINT "  Entries 16-255: repeat modulo 16"
570 PRINT
580 PRINT "16x16 grid: each row repeats the same"
590 PRINT "16 base colors (modulo pattern)."
600 PRINT
610 PRINT "Press any key to view..."
620 GETKEY K$
630 GRAPHIC 9
635 FAST
640 COLOR 0, 12 : SCNCLR : REM dark grey background
645 COLOR 0, 1 : COLOR 1, 2 : COLOR 2, 3 : COLOR 3, 4
650 GOSUB 9000
655 SLOW
660 GETKEY K$
670 GRAPHIC 0
675 GOSUB 8500 : REM restore text colors
700 REM ======== Page 4: Color Wheel ========
710 SCNCLR
720 PRINT "Page 3: Color Wheel"
730 PRINT "-------------------"
740 PRINT "256 entries = 16 hues x 16 brightness:"
750 PRINT "  Columns = hues around the color wheel"
760 PRINT "  Rows    = dark (top) to bright (bottom)"
770 PRINT
780 PRINT "Full spectrum from red through yellow,"
790 PRINT "green, cyan, blue, magenta and back."
800 PRINT
810 PRINT "Press any key to view..."
820 GETKEY K$
830 GRAPHIC 9
835 FAST
840 REM Build color wheel: 16 hues x 16 brightness
850 FOR C = 0 TO 15
860 HH = C * 22.5 : VL = 255
870 GOSUB 9200 : REM get HR, HG, HB
880 FOR L = 0 TO 15
890 E = C * 16 + L
900 BF = L + 1
910 R = INT(HR * BF / 16)
920 G = INT(HG * BF / 16)
930 B = INT(HB * BF / 16)
940 SETCOLOR E, R, G, B
950 NEXT L
960 NEXT C
970 COLOR 0, 1 : SCNCLR : REM black background
975 COLOR 1, 2 : COLOR 2, 3 : COLOR 3, 4
980 BW = 36 : BH = 22 : GP = 2
985 SX = INT((640 - 16 * BW - 15 * GP) / 2)
990 SY = INT((400 - 16 * BH - 15 * GP) / 2)
1000 FOR C = 0 TO 15
1010 FOR L = 0 TO 15
1020 E = C * 16 + L
1030 X1 = SX + C * (BW + GP) : Y1 = SY + L * (BH + GP)
1040 X2 = X1 + BW - 1 : Y2 = Y1 + BH - 1
1050 BOX E, X1, Y1, X2, Y2, 0, 1
1060 NEXT L
1070 NEXT C
1075 SLOW
1080 GETKEY K$
1090 GRAPHIC 0
1095 PRST : REM reset palette to C64 default
1098 GOSUB 8500 : REM restore text colors
1100 REM ======== Page 5: VGA 256-Color Palette ========
1110 SCNCLR
1120 PRINT "Page 4: Standard VGA 256-Color Palette"
1130 PRINT "--------------------------------------"
1140 PRINT "The classic VGA Mode 13h palette:"
1150 PRINT "  Entries 0-15:   16 EGA base colors"
1160 PRINT "  Entries 16-31:  16-step greyscale ramp"
1170 PRINT "  Entries 32-247: 3 rings x 12 hues x 6 sat"
1180 PRINT "  Entries 248-255: black"
1190 PRINT
1200 PRINT "Press any key to view..."
1210 GETKEY K$
1220 GRAPHIC 9
1225 FAST
1230 REM Build VGA palette: 16 EGA colors
1235 RESTORE 9700
1240 FOR I = 0 TO 15
1245 READ R, G, B
1250 SETCOLOR I, R, G, B
1255 NEXT I
1260 REM Greyscale ramp 16-31
1270 FOR I = 16 TO 31
1280 GV = INT((I - 16) * 255 / 15)
1290 SETCOLOR I, GV, GV, GV
1295 NEXT I
1300 REM 3 rings x 12 hues x 6 saturations (32-247)
1310 FOR I = 32 TO 247
1315 J = I - 32
1320 RG = INT(J / 72)
1325 PS = J - RG * 72
1330 HU = INT(PS / 6)
1335 ST = PS - HU * 6
1340 IF RG = 0 THEN MX = 255
1345 IF RG = 1 THEN MX = 180
1350 IF RG = 2 THEN MX = 126
1355 HH = HU * 30 : VL = MX
1360 GOSUB 9200 : REM get HR, HG, HB at full saturation
1365 REM Desaturate: interpolate toward grey (MX,MX,MX)
1370 R = INT(HR + (MX - HR) * ST / 5)
1375 G = INT(HG + (MX - HG) * ST / 5)
1380 B = INT(HB + (MX - HB) * ST / 5)
1385 SETCOLOR I, R, G, B
1390 NEXT I
1392 REM Entries 248-255: black
1394 FOR I = 248 TO 255
1396 SETCOLOR I, 0, 0, 0
1398 NEXT I
1400 COLOR 0, 1 : SCNCLR : REM black background
1405 COLOR 1, 2 : COLOR 2, 3 : COLOR 3, 4
1410 GOSUB 9000
1415 SLOW
1420 GETKEY K$
1430 GRAPHIC 0
1435 PRST : REM reset palette to C64 default
1440 GOSUB 8500 : REM restore text colors
1500 REM ======== Page 6: Rainbow Gradient ========
1510 SCNCLR
1520 PRINT "Page 5: Smooth Rainbow Gradient"
1530 PRINT "-------------------------------"
1540 PRINT "All 256 palette entries create a smooth"
1550 PRINT "rainbow spectrum across the full screen."
1560 PRINT
1570 PRINT "Each of the 256 vertical bars uses a"
1580 PRINT "unique color from red through violet."
1590 PRINT
1600 PRINT "Press any key to view..."
1610 GETKEY K$
1620 GRAPHIC 9
1625 FAST
1630 REM Build full rainbow palette (entries 0-255)
1640 FOR I = 0 TO 255
1650 HH = I * 360 / 256
1660 VL = 255
1670 GOSUB 9200 : REM get HR, HG, HB
1680 SETCOLOR I, HR, HG, HB
1690 NEXT I
1700 COLOR 0, 1 : SCNCLR : REM black background
1705 COLOR 1, 2 : COLOR 2, 3 : COLOR 3, 4
1710 REM Draw 256 vertical bars across 640 pixels
1720 BW = 2
1730 SX = INT((640 - 256 * BW) / 2)
1740 FOR I = 0 TO 255
1750 X1 = SX + I * BW : X2 = X1 + BW - 1
1760 BOX I, X1, 20, X2, 380, 0, 1
1770 NEXT I
1780 SLOW
1790 GETKEY K$
1800 GRAPHIC 0
1805 PRST : REM reset palette to C64 default
1810 GOSUB 8500 : REM restore text colors
1900 REM ======== Exit ========
1910 SCNCLR
1920 PRINT "============================================"
1930 PRINT "     Color & Palette Demo Complete!"
1940 PRINT "============================================"
1950 PRINT
1960 PRINT "The palette has been restored to defaults."
1970 PRINT
1980 PRINT "Thank you for watching!"
1990 END
8500 REM --- Restore text-mode color sources ---
8510 COLOR 0, 12 : COLOR 1, 14 : COLOR 4, 14
8520 RETURN
9000 REM --- Draw 16x16 color grid (640x400) ---
9010 BW = 36 : BH = 22 : GP = 2
9020 SX = INT((640 - 16 * BW - 15 * GP) / 2)
9030 SY = INT((400 - 16 * BH - 15 * GP) / 2)
9040 FOR I = 0 TO 255
9050 RW = INT(I / 16) : CL = I - RW * 16
9060 X1 = SX + CL * (BW + GP) : Y1 = SY + RW * (BH + GP)
9070 X2 = X1 + BW - 1 : Y2 = Y1 + BH - 1
9080 BOX I, X1, Y1, X2, Y2, 0, 1
9090 NEXT I
9100 RETURN
9200 REM --- HSV to RGB (S=1) subroutine ---
9210 REM Input: HH (hue 0-360), VL (max value)
9220 REM Output: HR, HG, HB
9230 SC = INT(HH / 60)
9240 FF = HH / 60 - SC
9250 IF SC = 0 THEN HR = VL : HG = INT(FF * VL) : HB = 0
9260 IF SC = 1 THEN HR = INT((1 - FF) * VL) : HG = VL : HB = 0
9270 IF SC = 2 THEN HR = 0 : HG = VL : HB = INT(FF * VL)
9280 IF SC = 3 THEN HR = 0 : HG = INT((1 - FF) * VL) : HB = VL
9290 IF SC = 4 THEN HR = INT(FF * VL) : HG = 0 : HB = VL
9300 IF SC = 5 THEN HR = VL : HG = 0 : HB = INT((1 - FF) * VL)
9310 IF SC >= 6 THEN HR = VL : HG = 0 : HB = 0
9320 RETURN
9700 REM === VGA/EGA 16 Standard Colors ===
9710 DATA 0, 0, 0
9720 DATA 0, 0, 170
9730 DATA 0, 170, 0
9740 DATA 0, 170, 170
9750 DATA 170, 0, 0
9760 DATA 170, 0, 170
9770 DATA 170, 85, 0
9780 DATA 170, 170, 170
9790 DATA 85, 85, 85
9800 DATA 85, 85, 255
9810 DATA 85, 255, 85
9820 DATA 85, 255, 255
9830 DATA 255, 85, 85
9840 DATA 255, 85, 255
9850 DATA 255, 255, 85
9860 DATA 255, 255, 255
