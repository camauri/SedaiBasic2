5 REM Initialize test counters
6 LET TP = 0: LET TF = 0
10 PRINT
20 PRINT "=============================================="
30 PRINT "   COMPLETE SEDAIBASIC FEATURE TEST SUITE"
40 PRINT "=============================================="
50 PRINT
60 PRINT "THIS TEST COVERS ALL IMPLEMENTED COMMANDS"
70 PRINT
80 GOSUB 9000
90 REM GOTO 5600

100 REM ========================================
110 REM *** ARITHMETIC OPERATORS TEST ***
120 REM ========================================
130 PRINT "TESTING ARITHMETIC OPERATORS"
140 LET A = 10: LET B = 3
150 PRINT "A = "; A; ", B = "; B
160 PRINT "A + B = "; A + B: TN$ = "ADD 10+3": EX = 13: AC = A + B: GOSUB 9500
170 PRINT "A - B = "; A - B: TN$ = "SUB 10-3": EX = 7: AC = A - B: GOSUB 9500
180 PRINT "A * B = "; A * B: TN$ = "MUL 10*3": EX = 30: AC = A * B: GOSUB 9500
190 PRINT "A / B = "; A / B: TN$ = "DIV 10/3": EX = 3: AC = INT(A / B): GOSUB 9500
200 PRINT "A ^ B (POWER) = "; A ^ B: TN$ = "POW 10^3": EX = 1000: AC = A ^ B: GOSUB 9500
210 PRINT "A MOD B = "; A MOD B: TN$ = "MOD 10%3": EX = 1: AC = A MOD B: GOSUB 9500
220 PRINT "------------------------------": PRINT
230 GOSUB 9000

300 REM ========================================
310 REM *** COMPARISON OPERATORS TEST ***
320 REM ========================================
330 PRINT "TESTING COMPARISON OPERATORS"
340 LET X = 5: LET Y = 10
350 PRINT "X = "; X; ", Y = "; Y
360 PRINT "X = Y: ";: IF X = Y THEN PRINT "TRUE": ELSE PRINT "FALSE"
361 TN$ = "5=10": EX = 0: AC = (X = Y): GOSUB 9500
370 PRINT "X < Y: ";: IF X < Y THEN PRINT "TRUE": ELSE PRINT "FALSE"
371 TN$ = "5<10": EX = -1: AC = (X < Y): GOSUB 9500
380 PRINT "X > Y: ";: IF X > Y THEN PRINT "TRUE": ELSE PRINT "FALSE"
381 TN$ = "5>10": EX = 0: AC = (X > Y): GOSUB 9500
390 PRINT "X <= Y: ";: IF X <= Y THEN PRINT "TRUE": ELSE PRINT "FALSE"
391 TN$ = "5<=10": EX = -1: AC = (X <= Y): GOSUB 9500
400 PRINT "X >= Y: ";: IF X >= Y THEN PRINT "TRUE": ELSE PRINT "FALSE"
401 TN$ = "5>=10": EX = 0: AC = (X >= Y): GOSUB 9500
410 PRINT "X <> Y: ";: IF X <> Y THEN PRINT "TRUE": ELSE PRINT "FALSE"
411 TN$ = "5<>10": EX = -1: AC = (X <> Y): GOSUB 9500
420 PRINT "------------------------------": PRINT
430 GOSUB 9000

500 REM ========================================
510 REM *** LOGICAL OPERATORS TEST ***
520 REM ========================================
530 PRINT "TESTING LOGICAL OPERATORS"
540 LET T = -1: LET F = 0
550 PRINT "T = "; T; " (TRUE=-1), F = "; F; " (FALSE=0)"
560 PRINT "T AND T = "; T AND T: TN$ = "T AND T": EX = -1: AC = T AND T: GOSUB 9500
570 PRINT "T AND F = "; T AND F: TN$ = "T AND F": EX = 0: AC = T AND F: GOSUB 9500
580 PRINT "T OR F = "; T OR F: TN$ = "T OR F": EX = -1: AC = T OR F: GOSUB 9500
590 PRINT "F OR F = "; F OR F: TN$ = "F OR F": EX = 0: AC = F OR F: GOSUB 9500
600 PRINT "NOT T = "; NOT T: TN$ = "NOT T": EX = 0: AC = NOT T: GOSUB 9500
610 PRINT "NOT F = "; NOT F: TN$ = "NOT F": EX = -1: AC = NOT F: GOSUB 9500
620 PRINT "T XOR F = "; T XOR F: TN$ = "T XOR F": EX = -1: AC = T XOR F: GOSUB 9500
630 PRINT "T XOR T = "; T XOR T: TN$ = "T XOR T": EX = 0: AC = T XOR T: GOSUB 9500
640 PRINT "------------------------------": PRINT
650 GOSUB 9000

700 REM ========================================
710 REM *** FLOW CONTROL - IF/THEN/ELSE ***
720 REM ========================================
730 PRINT "TESTING IF/THEN/ELSE (N = 42)"
740 LET N = 42
745 LET IP = 0: LET NN = 0: LET EQ = 0
750 IF N > 0 THEN IP = 1: PRINT "N IS POSITIVE"
760 IF N < 0 THEN PRINT "N IS NEGATIVE" ELSE NN = 1: PRINT "N IS NOT NEGATIVE"
770 IF N = 42 THEN EQ = 1: PRINT "N EQUALS 42" ELSE PRINT "N DOES NOT EQUAL 42"
775 TN$ = "IF N>0": EX = 1: AC = IP: GOSUB 9500
776 TN$ = "IF ELSE": EX = 1: AC = NN: GOSUB 9500
777 TN$ = "IF N=42": EX = 1: AC = EQ: GOSUB 9500
780 PRINT "------------------------------": PRINT
790 GOSUB 9000

800 REM ========================================
810 REM *** FLOW CONTROL - GOTO/GOSUB ***
820 REM ========================================
830 PRINT "TESTING GOTO AND GOSUB"
840 PRINT "JUMPING TO LINE 860..."
845 LET GF = 0
850 GOTO 860
855 GF = 99: PRINT "THIS LINE SHOULD BE SKIPPED"
860 PRINT "GOTO WORKED!"
861 TN$ = "GOTO skip": EX = 0: AC = GF: GOSUB 9500
870 PRINT "CALLING SUBROUTINE..."
880 GOSUB 8500
890 PRINT "RETURNED FROM SUBROUTINE"
891 TN$ = "GOSUB/RET": EX = 1: AC = GR: GOSUB 9500
900 PRINT "------------------------------": PRINT
910 GOSUB 9000

1000 REM ========================================
1010 REM *** ON GOTO/GOSUB TEST ***
1020 REM ========================================
1030 PRINT "TESTING ON...GOTO/GOSUB"
1040 LET C = 2: LET OG = 0
1050 PRINT "C = "; C
1060 ON C GOTO 1080, 1090, 1100
1070 GOTO 1110
1080 OG = 1: PRINT "C = 1": GOTO 1110
1090 OG = 2: PRINT "C = 2 (CORRECT!)": GOTO 1110
1100 OG = 3: PRINT "C = 3": GOTO 1110
1110 PRINT "ON GOSUB TEST:"
1111 TN$ = "ON GOTO": EX = 2: AC = OG: GOSUB 9500
1120 ON C GOSUB 8600, 8610, 8620
1130 PRINT "------------------------------": PRINT
1140 GOSUB 9000

1200 REM ========================================
1210 REM *** FOR/NEXT LOOPS ***
1220 REM ========================================
1230 PRINT "TESTING FOR/NEXT LOOPS"
1240 PRINT "COUNTING 1 TO 5:"
1245 LET SM = 0
1250 FOR I = 1 TO 5
1260 PRINT "  I = "; I: SM = SM + I
1270 NEXT I
1275 TN$ = "FOR 1-5 sum": EX = 15: AC = SM: GOSUB 9500
1280 PRINT "COUNTING 10 TO 2 STEP -2:"
1285 SM = 0
1290 FOR J = 10 TO 2 STEP -2
1300 PRINT "  J = "; J: SM = SM + J
1310 NEXT J
1315 TN$ = "FOR STEP-2": EX = 30: AC = SM: GOSUB 9500
1320 PRINT "NESTED LOOPS:"
1325 SM = 0
1330 FOR A = 1 TO 2
1340 FOR B = 1 TO 3
1350 PRINT "  A="; A; " B="; B: SM = SM + 1
1360 NEXT B
1370 NEXT A
1375 TN$ = "NESTED 2x3": EX = 6: AC = SM: GOSUB 9500
1380 PRINT "------------------------------": PRINT
1390 GOSUB 9000

1500 REM ========================================
1510 REM *** DO/LOOP WITH WHILE/UNTIL ***
1520 REM ========================================
1530 PRINT "TESTING DO/LOOP"
1540 LET K = 1
1550 PRINT "DO UNTIL K=4:"
1560 DO UNTIL K = 4
1570 PRINT "  K = "; K
1580 K = K + 1
1590 LOOP
1595 TN$ = "DO UNTIL": EX = 4: AC = K: GOSUB 9500
1600 K = 5
1610 PRINT "DO WHILE K>1:"
1620 DO WHILE K > 1
1630 PRINT "  K = "; K
1640 K = K - 1
1650 LOOP
1655 TN$ = "DO WHILE": EX = 1: AC = K: GOSUB 9500
1660 PRINT "DO/LOOP WITH UNTIL AT END:"
1670 K = 1
1680 DO
1690 PRINT "  K = "; K
1700 K = K + 1
1710 LOOP UNTIL K > 3
1715 TN$ = "LOOP UNTIL": EX = 4: AC = K: GOSUB 9500
1720 PRINT "DO/LOOP WITH WHILE AT END:"
1730 K = 3
1740 DO
1750 PRINT "  K = "; K
1760 K = K - 1
1770 LOOP WHILE K >= 1
1775 TN$ = "LOOP WHILE": EX = 0: AC = K: GOSUB 9500
1780 PRINT "------------------------------": PRINT
1790 GOSUB 9000

1800 REM ========================================
1810 REM *** EXIT STATEMENT ***
1820 REM ========================================
1830 PRINT "TESTING EXIT"
1840 PRINT "FOR LOOP WITH EXIT:"
1850 FOR I = 1 TO 100
1860 PRINT "  I = "; I
1870 IF I = 3 THEN EXIT
1880 NEXT I
1890 PRINT "EXITED AT I = 3"
1891 TN$ = "EXIT FOR": EX = 3: AC = I: GOSUB 9500
1900 PRINT "DO LOOP WITH EXIT:"
1910 K = 1
1920 DO
1930 PRINT "  K = "; K
1940 K = K + 1
1950 IF K > 3 THEN EXIT
1960 LOOP
1970 PRINT "EXITED DO LOOP AT K = "; K
1971 TN$ = "EXIT DO": EX = 4: AC = K: GOSUB 9500
1980 PRINT "------------------------------": PRINT
1990 GOSUB 9000

2000 REM ========================================
2010 REM *** CODE BLOCKS (BEGIN/BEND) ***
2020 REM ========================================
2030 PRINT "TESTING CODE BLOCKS (BEGIN/BEND)"
2040 LET V = 10
2050 IF V > 5 THEN BEGIN
2060 PRINT "  INSIDE BEGIN BLOCK"
2070 PRINT "  V = "; V
2080 V = V * 2
2090 PRINT "  V DOUBLED = "; V
2100 BEND
2110 PRINT "AFTER BEND, V = "; V
2111 TN$ = "BEGIN/BEND": EX = 20: AC = V: GOSUB 9500
2120 PRINT "------------------------------": PRINT
2130 GOSUB 9000

2200 REM ========================================
2210 REM *** USER DEFINED FUNCTIONS (DEF FN) ***
2220 REM ========================================
2230 PRINT "TESTING DEF FN"
2240 DEF FNSQ(X) = X * X
2250 DEF FNCU(X) = X * X * X
2260 DEF FNDB(X) = X * 2
2270 PRINT "FNSQ(5) = "; FNSQ(5): TN$ = "FNSQ(5)": EX = 25: AC = FNSQ(5): GOSUB 9500
2280 PRINT "FNCU(3) = "; FNCU(3): TN$ = "FNCU(3)": EX = 27: AC = FNCU(3): GOSUB 9500
2290 PRINT "FNDB(7) = "; FNDB(7): TN$ = "FNDB(7)": EX = 14: AC = FNDB(7): GOSUB 9500
2300 PRINT "------------------------------": PRINT
2310 GOSUB 9000

2400 REM ========================================
2410 REM *** DATA MANAGEMENT ***
2420 REM ========================================
2430 PRINT "TESTING DATA/READ/RESTORE"
2440 DATA 10, 20, 30, "HELLO", "WORLD"
2450 READ D1, D2, D3
2460 PRINT "READ NUMBERS: "; D1; ", "; D2; ", "; D3
2461 TN$ = "READ D1": EX = 10: AC = D1: GOSUB 9500
2462 TN$ = "READ D2": EX = 20: AC = D2: GOSUB 9500
2463 TN$ = "READ D3": EX = 30: AC = D3: GOSUB 9500
2470 READ D$, E$
2480 PRINT "READ STRINGS: "; D$; ", "; E$
2481 TN$ = "READ D$": EXS$ = "HELLO": ACS$ = D$: GOSUB 9550
2482 TN$ = "READ E$": EXS$ = "WORLD": ACS$ = E$: GOSUB 9550
2490 RESTORE
2500 READ R1
2510 PRINT "AFTER RESTORE, READ: "; R1
2511 TN$ = "RESTORE": EX = 10: AC = R1: GOSUB 9500
2520 PRINT "------------------------------": PRINT
2530 GOSUB 9000

2600 REM ========================================
2610 REM *** DIM - ARRAYS ***
2620 REM ========================================
2630 PRINT "TESTING DIM ARRAYS"
2640 DIM AR(10), AR2(5, 5), AR3(3, 3, 3)
2650 FOR I = 1 TO 10: AR(I) = I * 10: NEXT I
2660 PRINT "AR(5) = "; AR(5): TN$ = "AR(5)": EX = 50: AC = AR(5): GOSUB 9500
2670 AR2(2, 3) = 123
2680 PRINT "AR2(2,3) = "; AR2(2, 3): TN$ = "AR2(2,3)": EX = 123: AC = AR2(2, 3): GOSUB 9500
2690 AR3(1, 2, 3) = 456
2700 PRINT "AR3(1,2,3) = "; AR3(1, 2, 3): TN$ = "AR3(1,2,3)": EX = 456: AC = AR3(1, 2, 3): GOSUB 9500
2710 PRINT "------------------------------": PRINT
2720 GOSUB 9000

2800 REM ========================================
2810 REM *** LET AND CLR ***
2820 REM ========================================
2830 PRINT "TESTING LET AND VARIABLE TYPES"
2840 LET FL = 3.14159
2850 LET IN% = 42
2860 LET ST$ = "HELLO SEDAIBASIC"
2870 PRINT "FLOAT FL = "; FL
2880 PRINT "INTEGER IN% = "; IN%: TN$ = "INT VAR": EX = 42: AC = IN%: GOSUB 9500
2890 PRINT "STRING ST$ = "; ST$
2891 TN$ = "STR VAR": EXS$ = "HELLO SEDAIBASIC": ACS$ = ST$: GOSUB 9550
2900 PRINT "------------------------------": PRINT
2910 GOSUB 9000

3000 REM ========================================
3010 REM *** STRING FUNCTIONS ***
3020 REM ========================================
3030 PRINT "TESTING STRING FUNCTIONS"
3040 LET S$ = "HELLO WORLD"
3050 PRINT "S$ = "; S$
3060 PRINT "LEN(S$) = "; LEN(S$): TN$ = "LEN": EX = 11: AC = LEN(S$): GOSUB 9500
3070 PRINT "LEFT$(S$,5) = "; LEFT$(S$, 5): TN$ = "LEFT$": EXS$ = "HELLO": ACS$ = LEFT$(S$, 5): GOSUB 9550
3080 PRINT "RIGHT$(S$,5) = "; RIGHT$(S$, 5): TN$ = "RIGHT$": EXS$ = "WORLD": ACS$ = RIGHT$(S$, 5): GOSUB 9550
3090 PRINT "MID$(S$,3,5) = "; MID$(S$, 3, 5): TN$ = "MID$": EXS$ = "LLO W": ACS$ = MID$(S$, 3, 5): GOSUB 9550
3100 PRINT "ASC(";CHR$(34);"A";CHR$(34);") = "; ASC("A"): TN$ = "ASC": EX = 65: AC = ASC("A"): GOSUB 9500
3110 PRINT "CHR$(65) = "; CHR$(65): TN$ = "CHR$": EXS$ = "A": ACS$ = CHR$(65): GOSUB 9550
3120 PRINT "STR$(123) = "; STR$(123)
3130 PRINT "VAL(";CHR$(34);"456";CHR$(34);") = "; VAL("456"): TN$ = "VAL": EX = 456: AC = VAL("456"): GOSUB 9500
3140 PRINT "INSTR(S$,";CHR$(34);"WORLD";CHR$(34);") = "; INSTR(S$, "WORLD"): TN$ = "INSTR": EX = 7: AC = INSTR(S$, "WORLD"): GOSUB 9500
3150 PRINT "INSTR(1,S$,";CHR$(34);"O";CHR$(34);") = "; INSTR(1, S$, "O")
3160 PRINT "HEX$(255) = "; HEX$(255): TN$ = "HEX$": EXS$ = "FF": ACS$ = HEX$(255): GOSUB 9550
3170 PRINT "DEC(";CHR$(34);"FF";CHR$(34);") = "; DEC("FF"): TN$ = "DEC": EX = 255: AC = DEC("FF"): GOSUB 9500
3180 PRINT "------------------------------": PRINT
3190 GOSUB 9000

3300 REM ========================================
3310 REM *** MATH FUNCTIONS ***
3320 REM ========================================
3330 PRINT "TESTING MATH FUNCTIONS"
3340 PRINT "ABS(-5) = "; ABS(-5): TN$ = "ABS": EX = 5: AC = ABS(-5): GOSUB 9500
3350 PRINT "SGN(-5) = "; SGN(-5): TN$ = "SGN-": EX = -1: AC = SGN(-5): GOSUB 9500
3360 PRINT "SGN(0) = "; SGN(0): TN$ = "SGN0": EX = 0: AC = SGN(0): GOSUB 9500
3370 PRINT "SGN(5) = "; SGN(5): TN$ = "SGN+": EX = 1: AC = SGN(5): GOSUB 9500
3380 PRINT "INT(3.7) = "; INT(3.7): TN$ = "INT+": EX = 3: AC = INT(3.7): GOSUB 9500
3390 PRINT "INT(-3.7) = "; INT(-3.7): TN$ = "INT-": EX = -4: AC = INT(-3.7): GOSUB 9500
3400 PRINT "SQR(16) = "; SQR(16): TN$ = "SQR": EX = 4: AC = SQR(16): GOSUB 9500
3410 PRINT "SIN(0) = "; SIN(0): TN$ = "SIN0": EX = 0: AC = INT(SIN(0)): GOSUB 9500
3420 PRINT "COS(0) = "; COS(0): TN$ = "COS0": EX = 1: AC = INT(COS(0)): GOSUB 9500
3430 PRINT "TAN(0) = "; TAN(0): TN$ = "TAN0": EX = 0: AC = INT(TAN(0)): GOSUB 9500
3440 PRINT "ATN(1) = "; ATN(1)
3450 PRINT "EXP(1) = "; EXP(1)
3460 PRINT "LOG(2.718) = "; LOG(2.718)
3470 PRINT "LOG10(100) = "; LOG10(100): TN$ = "LOG10": EX = 2: AC = INT(LOG10(100)): GOSUB 9500
3480 PRINT "LOG2(8) = "; LOG2(8): TN$ = "LOG2": EX = 3: AC = INT(LOG2(8) + 0.5): GOSUB 9500
3490 RV = RND(0): PRINT "RND(0) = "; RV
3495 TN$ = "RND range": EX = 1: AC = 0: IF RV >= 0.000000001 AND RV <= 0.999999999 THEN AC = 1
3496 GOSUB 9500
3500 PRINT "------------------------------": PRINT
3510 GOSUB 9000

3600 REM ========================================
3610 REM *** PRINT USING ***
3620 REM ========================================
3630 PRINT "TESTING PRINT USING"
3640 LET V = 1234.56
3650 PRINT USING "######.##"; V
3660 PRINT USING "#$####.##"; 99.99
3670 PRINT USING "###,###.##"; 12345.67
3680 PRINT "PUDEF TEST (CHANGE FILLER TO *):"
3690 PUDEF "*"
3700 PRINT USING "######.##"; 12.34
3710 PUDEF " "
3720 PRINT "------------------------------": PRINT
3730 GOSUB 9000

3800 REM ========================================
3810 REM *** TAB AND SPC ***
3820 REM ========================================
3830 PRINT "TESTING TAB AND SPC"
3840 PRINT "START"; TAB(20); "TABBED TO 20"
3850 PRINT "ONE"; SPC(10); "TEN SPACES"; SPC(5); "FIVE"
3860 PRINT "------------------------------": PRINT
3870 GOSUB 9000

4000 REM ========================================
4010 REM *** ERROR HANDLING (TRAP/RESUME) ***
4020 REM ========================================
4030 PRINT "TESTING ERROR HANDLING"
4031 REM
4032 REM === TEST 1: RESUME NEXT ===
4033 PRINT "=== TEST 1: RESUME NEXT ==="
4035 LET EC = 0
4040 TRAP 4100
4050 PRINT "TRAP SET TO LINE 4100"
4060 PRINT "DIVIDING BY ZERO..."
4070 LET Z = 1 / 0
4075 EC = 1
4080 PRINT "LINE AFTER ERROR (RESUME NEXT worked)"
4090 GOTO 4200
4100 REM ERROR HANDLER 1
4110 PRINT "ERROR CAUGHT!"
4120 PRINT "ERROR LINE (EL) = "; EL
4130 PRINT "ERROR CODE (ER) = "; ER
4140 PRINT "ERROR MESSAGE: "; ERR$(ER)
4150 TRAP 0
4160 RESUME NEXT
4200 TN$ = "RESUME NEXT": EX = 1: AC = EC: GOSUB 9500
4205 PRINT
4210 REM === TEST 2: RESUME <LINE> ===
4211 PRINT "=== TEST 2: RESUME <LINE> ==="
4212 LET EC = 0
4215 TRAP 4300
4220 PRINT "TRAP SET TO LINE 4300"
4230 PRINT "DIVIDING BY ZERO AGAIN..."
4240 DIM AB(1): PRINT AB(2): REM LET Y = 1 / 0
4250 PRINT "THIS SHOULD NOT PRINT (1)"
4260 GOTO 4350
4300 REM ERROR HANDLER 2
4310 PRINT "ERROR CAUGHT!"
4315 EC = 1
4320 PRINT "ERROR LINE (EL) = "; EL
4325 PRINT "ERROR CODE (ER) = "; ER
4330 PRINT "ERROR MESSAGE: "; ERR$(ER)
4335 TRAP 0
4340 RESUME 4350
4345 PRINT "THIS SHOULD NOT PRINT (2)"
4350 PRINT "JUMPED TO 4350 (RESUME <LINE> worked)"
4360 TN$ = "RESUME LINE": EX = 1: AC = EC: GOSUB 9500
4370 PRINT "------------------------------": PRINT
4380 GOSUB 9000

4400 REM ========================================
4410 REM *** RESERVED VARIABLES ***
4420 REM ========================================
4430 PRINT "TESTING RESERVED VARIABLES"
4440 PRINT "TI (JIFFIES) = "; TI
4450 PRINT "TI$ (TIME) = "; TI$
4460 PRINT "DT$ (DATE) = "; DT$
4470 PRINT "FRE(0) = "; FRE(0)
4480 PRINT "------------------------------": PRINT
4490 GOSUB 9000

4600 REM ========================================
4610 REM *** SLEEP AND TIMING ***
4620 REM ========================================
4630 PRINT "TESTING SLEEP (1 SECOND)"
4640 T1 = TI
4650 SLEEP 1
4660 T2 = TI
4670 PRINT "ELAPSED JIFFIES: "; T2 - T1
4675 TN$ = "SLEEP 1s": EX = 1: AC = INT((T2 - T1) / 50): GOSUB 9500
4680 PRINT "------------------------------": PRINT
4690 GOSUB 9000

4800 REM ========================================
4810 REM *** GET (NON-BLOCKING INPUT) ***
4820 REM ========================================
4830 PRINT "TESTING GET (NON-BLOCKING)"
4840 GET G$
4850 IF G$ = "" THEN PRINT "NO KEY PRESSED (EXPECTED)"
4860 IF G$ <> "" THEN PRINT "KEY PRESSED: "; G$
4870 PRINT "------------------------------": PRINT
4880 GOSUB 9000

5000 REM ========================================
5010 REM *** GETKEY (BLOCKING INPUT) ***
5020 REM ========================================
5030 PRINT "TESTING GETKEY (BLOCKING)"
5040 PRINT "PRESS ANY KEY..."
5050 GETKEY K$
5060 PRINT "YOU PRESSED: "; K$
5070 PRINT "------------------------------": PRINT
5080 GOSUB 9000

5200 REM ========================================
5210 REM *** INPUT STATEMENT ***
5220 REM ========================================
5230 PRINT "TESTING INPUT"
5240 INPUT "ENTER A NUMBER: "; UN
5250 PRINT "YOU ENTERED: "; UN
5260 PRINT "DOUBLED: "; UN * 2
5270 INPUT "ENTER A STRING: "; US$
5280 PRINT "YOU ENTERED: "; US$
5290 PRINT "LENGTH: "; LEN(US$)
5300 PRINT "------------------------------": PRINT
5310 GOSUB 9000

5400 REM ========================================
5410 REM *** FAST/SLOW MODE ***
5420 REM ========================================
5430 PRINT "TESTING FAST/SLOW MODE"
5440 PRINT "SWITCHING TO FAST MODE..."
5450 FAST
5460 FOR I = 1 TO 1000: NEXT I
5470 SLOW
5480 PRINT "BACK TO SLOW MODE"
5490 PRINT "------------------------------": PRINT
5500 GOSUB 9000

5600 REM ========================================
5610 REM *** POS FUNCTION ***
5620 REM ========================================
5630 PRINT "TESTING POS FUNCTION"
5640 PRINT "CURSOR POSITION:";
5650 PRINT " POS(0) = "; POS(0)
5660 PRINT "------------------------------": PRINT
5670 GOSUB 9000

5800 REM ========================================
5810 REM *** CONST STATEMENT ***
5820 REM ========================================
5830 PRINT "TESTING CONST"
5840 CONST PI = 3.14159
5850 CONST GR$ = "SEDAIBASIC"
5860 PRINT "PI = "; PI
5870 PRINT "GR$ = "; GR$: TN$ = "CONST$": EXS$ = "SEDAIBASIC": ACS$ = GR$: GOSUB 9550
5880 PRINT "------------------------------": PRINT
5890 GOSUB 9000

6000 REM ========================================
6010 REM *** KEY FUNCTION ***
6020 REM ========================================
6030 PRINT "TESTING KEY DEFINITION"
6040 KEY 1, "LIST"
6050 PRINT "F1 KEY DEFINED AS 'LIST'"
6060 KEY
6070 PRINT "------------------------------": PRINT
6080 GOSUB 9000

6200 REM ========================================
6210 REM *** RENUMBER TEST ***
6220 REM ========================================
6230 PRINT "RENUMBER COMMAND AVAILABLE"
6240 PRINT "(SHELL COMMAND - NOT TESTED IN PROGRAM)"
6250 PRINT "------------------------------": PRINT
6260 GOSUB 9000

6400 REM ========================================
6410 REM *** DELETE TEST ***
6420 REM ========================================
6430 PRINT "DELETE COMMAND AVAILABLE"
6440 PRINT "(SHELL COMMAND - NOT TESTED IN PROGRAM)"
6450 PRINT "------------------------------": PRINT
6460 GOSUB 9000

6600 REM ========================================
6610 REM *** AUTO LINE NUMBERING ***
6620 REM ========================================
6630 PRINT "AUTO COMMAND AVAILABLE"
6640 PRINT "(SHELL COMMAND - NOT TESTED IN PROGRAM)"
6650 PRINT "------------------------------": PRINT
6660 GOSUB 9000

6800 REM ========================================
6810 REM *** REM - COMMENTS ***
6820 REM ========================================
6830 PRINT "TESTING REM (COMMENTS)"
6840 REM THIS IS A COMMENT AND SHOULD NOT AFFECT EXECUTION
6850 PRINT "REM STATEMENT WORKS CORRECTLY"
6860 PRINT "------------------------------": PRINT
6870 GOSUB 9000

7000 REM ========================================
7010 REM *** TEST COMPLETE - FINAL REPORT ***
7020 REM ========================================
7030 PRINT
7040 PRINT "=============================================="
7050 PRINT "            TEST RESULTS SUMMARY"
7060 PRINT "=============================================="
7070 PRINT
7080 PRINT "TESTS PASSED: "; TP
7090 PRINT "TESTS FAILED: "; TF
7100 PRINT "TOTAL TESTS:  "; TP + TF
7110 PRINT
7120 IF TF = 0 THEN PRINT "*** ALL TESTS PASSED! ***"
7130 IF TF > 0 THEN PRINT "*** SOME TESTS FAILED! ***"
7140 PRINT
7150 PRINT "=============================================="
7160 END

8500 REM *** GOSUB SUBROUTINE ***
8510 PRINT "  INSIDE GOSUB SUBROUTINE"
8515 GR = 1
8520 RETURN

8600 PRINT "  ON GOSUB TARGET 1": RETURN
8610 PRINT "  ON GOSUB TARGET 2 (CORRECT!)": RETURN
8620 PRINT "  ON GOSUB TARGET 3": RETURN

9000 REM *** PAUSE SUBROUTINE ***
9010 INPUT "PRESS RETURN TO CONTINUE, Q+RETURN TO QUIT"; PA$
9020 IF PA$ = "Q" OR PA$ = "q" THEN GOTO 7000
9030 PRINT
9040 RETURN

9500 REM *** Assert numeric equality ***
9510 REM EX = expected, AC = actual, TN$ = test name
9520 IF EX = AC THEN GOTO 9560
9530 PRINT "  [FAIL] "; TN$; " expected "; EX; " got "; AC
9540 TF = TF + 1
9545 RETURN
9560 PRINT "  [PASS] "; TN$
9570 TP = TP + 1
9580 RETURN

9550 REM *** Assert string equality ***
9551 REM EXS$ = expected, ACS$ = actual, TN$ = test name
9552 IF EXS$ = ACS$ THEN GOTO 9556
9553 PRINT "  [FAIL] "; TN$; " expected '"; EXS$; "' got '"; ACS$; "'"
9554 TF = TF + 1
9555 RETURN
9556 PRINT "  [PASS] "; TN$
9557 TP = TP + 1
9558 RETURN
