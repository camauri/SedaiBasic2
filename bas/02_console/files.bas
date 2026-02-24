10 REM =============================================
20 REM  SedaiBasic File I/O Test Suite
30 REM  Tests: DOPEN, DCLOSE, PRINT#, INPUT#, GET#, CMD,
35 REM         APPEND, DCLEAR, RECORD
40 REM =============================================
50 SCNCLR
55 PRINT
60 PRINT "=== FILE I/O TEST SUITE ==="
65 PRINT
70 PRINT "Current directory: "; CWD$
75 TESTFILE$ = "testfile.txt"
80 PRINT "Test filename: "; TESTFILE$
85 TP% = 0 : TF% = 0
90 GOSUB 9700

100 REM =============================================
110 REM  TEST 1: Write to File
120 REM =============================================
130 PRINT "TEST 1: Write to File (DOPEN/PRINT#)"
140 PRINT "------------------------------------------"
150 DOPEN #1, TESTFILE$, "W"
160 PRINT# 1, "Hello from SedaiBasic!"
170 PRINT# 1, "Line 2: Numbers follow"
180 PRINT# 1, 100
190 PRINT# 1, 3.14159
200 PRINT# 1, "Final line"
210 DCLOSE #1
220 PRINT "File written successfully"
230 TP% = TP% + 1
240 GOSUB 9700

300 REM =============================================
310 REM  TEST 2: Read from File (INPUT#)
320 REM =============================================
330 PRINT "TEST 2: Read from File (INPUT#)"
340 PRINT "------------------------------------------"
350 DOPEN #1, TESTFILE$, "R"
360 INPUT# 1, L1$
370 INPUT# 1, L2$
380 INPUT# 1, N1
390 INPUT# 1, N2
400 INPUT# 1, L3$
410 DCLOSE #1
420 PRINT "Line 1: "; L1$
430 PRINT "Line 2: "; L2$
440 PRINT "Number 1: "; N1
450 PRINT "Number 2: "; N2
460 PRINT "Line 3: "; L3$
470 TN$ = "Read N1": EX = 100: AC = N1: GOSUB 9500
480 GOSUB 9700

600 REM =============================================
610 REM  TEST 3: GET# (Character by character)
620 REM =============================================
630 PRINT "TEST 3: Read Character by Character (GET#)"
640 PRINT "------------------------------------------"
650 DOPEN #1, TESTFILE$, "R"
660 PRINT "First 20 characters: ";
670 FOR I = 1 TO 20
680 GET# 1, C$
690 IF C$ <> "" THEN PRINT C$;
700 NEXT I
710 DCLOSE #1
720 PRINT
730 TP% = TP% + 1
740 GOSUB 9700

800 REM =============================================
810 REM  TEST 4: Append Mode
820 REM =============================================
830 PRINT "TEST 4: Append Mode"
840 PRINT "------------------------------------------"
850 DOPEN #1, TESTFILE$, "A"
860 PRINT# 1, "Appended line 1"
870 PRINT# 1, "Appended line 2"
880 DCLOSE #1
890 PRINT "Lines appended"
900 REM Verify
910 DOPEN #1, TESTFILE$, "R"
920 LC% = 0
930 TRAP 970
940 INPUT# 1, DUMMY$
950 LC% = LC% + 1
960 GOTO 940
970 TRAP 0
975 DCLOSE #1
980 PRINT "Total lines in file: "; LC%
990 TN$ = "Line count after append": EX = 7: AC = LC%: GOSUB 9500
1000 GOSUB 9700

1100 REM =============================================
1110 REM  TEST 5: Multiple File Handles
1120 REM =============================================
1130 PRINT "TEST 5: Multiple File Handles"
1140 PRINT "------------------------------------------"
1150 DOPEN #1, "file1.txt", "W"
1160 DOPEN #2, "file2.txt", "W"
1170 PRINT# 1, "Content for file 1"
1180 PRINT# 2, "Content for file 2"
1190 DCLOSE #1
1200 DCLOSE #2
1210 REM Verify
1220 DOPEN #1, "file1.txt", "R"
1230 INPUT# 1, F1$
1240 DCLOSE #1
1250 DOPEN #2, "file2.txt", "R"
1260 INPUT# 2, F2$
1270 DCLOSE #2
1280 PRINT "File 1: "; F1$
1290 PRINT "File 2: "; F2$
1300 IF INSTR(F1$, "file 1") > 0 AND INSTR(F2$, "file 2") > 0 THEN TP% = TP% + 1: PRINT "  [PASS] Multiple handles"
1310 IF INSTR(F1$, "file 1") = 0 OR INSTR(F2$, "file 2") = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] Multiple handles"
1320 GOSUB 9700

1400 REM =============================================
1410 REM  TEST 6: CMD Redirection
1420 REM =============================================
1430 PRINT "TEST 6: CMD Output Redirection"
1440 PRINT "------------------------------------------"
1450 DOPEN #3, "cmdtest.txt", "W"
1460 CMD 3
1470 PRINT "This goes to file via CMD"
1480 PRINT "Second line via CMD"
1490 PRINT# 3
1500 DCLOSE #3
1510 PRINT "CMD redirection complete"
1520 REM Verify
1530 DOPEN #3, "cmdtest.txt", "R"
1540 INPUT# 3, CMD1$
1550 DCLOSE #3
1560 PRINT "Read back: "; CMD1$
1570 IF INSTR(CMD1$, "CMD") > 0 THEN TP% = TP% + 1: PRINT "  [PASS] CMD redirection"
1580 IF INSTR(CMD1$, "CMD") = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] CMD redirection"
1590 GOSUB 9700

1700 REM =============================================
1710 REM  TEST 7: OPEN/CLOSE Aliases
1720 REM =============================================
1730 PRINT "TEST 7: OPEN/CLOSE Aliases"
1740 PRINT "------------------------------------------"
1750 OPEN #1, "alias.txt", "W"
1760 PRINT# 1, "Testing OPEN alias"
1770 CLOSE #1
1780 OPEN #1, "alias.txt", "R"
1790 INPUT# 1, AL$
1800 CLOSE #1
1810 PRINT "Read: "; AL$
1820 IF INSTR(AL$, "OPEN") > 0 THEN TP% = TP% + 1: PRINT "  [PASS] OPEN/CLOSE aliases"
1830 IF INSTR(AL$, "OPEN") = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] OPEN/CLOSE aliases"
1840 GOSUB 9700

2000 REM =============================================
2010 REM  TEST 8: APPEND Command
2020 REM =============================================
2030 PRINT "TEST 8: APPEND Command"
2040 PRINT "------------------------------------------"
2050 DOPEN #1, "append.txt", "W"
2060 PRINT# 1, "First line"
2070 DCLOSE #1
2080 REM Now use APPEND to add more data
2090 DOPEN #1, "append.txt", "A"
2100 APPEND #1, "Appended with APPEND command"
2110 APPEND #1, CHR$(13) + CHR$(10)
2120 APPEND #1, "Second appended line"
2130 DCLOSE #1
2140 REM Verify
2150 DOPEN #1, "append.txt", "R"
2160 INPUT# 1, AP1$
2170 INPUT# 1, AP2$
2180 INPUT# 1, AP3$
2190 DCLOSE #1
2200 PRINT "Line 1: "; AP1$
2210 PRINT "Line 2: "; AP2$
2220 PRINT "Line 3: "; AP3$
2230 IF INSTR(AP2$, "APPEND") > 0 THEN TP% = TP% + 1: PRINT "  [PASS] APPEND command"
2240 IF INSTR(AP2$, "APPEND") = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] APPEND command"
2250 GOSUB 9700

2300 REM =============================================
2310 REM  TEST 9: DCLEAR Command
2320 REM =============================================
2330 PRINT "TEST 9: DCLEAR Command"
2340 PRINT "------------------------------------------"
2350 REM Open multiple files
2360 DOPEN #1, "dclear1.txt", "W"
2370 DOPEN #2, "dclear2.txt", "W"
2380 DOPEN #3, "dclear3.txt", "W"
2390 PRINT# 1, "File 1 content"
2400 PRINT# 2, "File 2 content"
2410 PRINT# 3, "File 3 content"
2420 REM Close all at once with DCLEAR
2430 DCLEAR
2440 PRINT "DCLEAR executed - all files closed"
2450 REM Verify files were written and closed properly
2460 DOPEN #1, "dclear1.txt", "R"
2470 INPUT# 1, DC1$
2480 DCLOSE #1
2490 DOPEN #2, "dclear2.txt", "R"
2500 INPUT# 2, DC2$
2510 DCLOSE #2
2520 PRINT "File 1: "; DC1$
2530 PRINT "File 2: "; DC2$
2540 IF INSTR(DC1$, "File 1") > 0 AND INSTR(DC2$, "File 2") > 0 THEN TP% = TP% + 1: PRINT "  [PASS] DCLEAR command"
2550 IF INSTR(DC1$, "File 1") = 0 OR INSTR(DC2$, "File 2") = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] DCLEAR command"
2560 GOSUB 9700

2600 REM =============================================
2610 REM  TEST 10: RECORD Command (File Seek)
2620 REM =============================================
2630 PRINT "TEST 10: RECORD Command (File Seek)"
2640 PRINT "------------------------------------------"
2650 REM Create a test file with known content
2660 DOPEN #1, "record.txt", "W"
2670 PRINT# 1, "ABCDEFGHIJ"
2680 PRINT# 1, "1234567890"
2690 DCLOSE #1
2700 REM Open for reading and use RECORD to seek
2710 DOPEN #1, "record.txt", "R"
2720 REM Read first char (should be 'A')
2730 GET# 1, R1$
2740 PRINT "Position 0: "; R1$
2750 REM Seek to position 5 (should be 'F')
2760 RECORD #1, 5
2770 GET# 1, R2$
2780 PRINT "Position 5: "; R2$
2790 REM Seek back to position 0
2800 RECORD #1, 0
2810 GET# 1, R3$
2820 PRINT "Back to 0: "; R3$
2830 DCLOSE #1
2840 IF R1$ = "A" AND R2$ = "F" AND R3$ = "A" THEN TP% = TP% + 1: PRINT "  [PASS] RECORD command"
2850 IF R1$ <> "A" OR R2$ <> "F" OR R3$ <> "A" THEN TF% = TF% + 1: PRINT "  [FAIL] RECORD command"
2860 GOSUB 9700

4000 REM =============================================
4010 REM  FINAL RESULTS
4020 REM =============================================
4030 PRINT "========================================="
4040 PRINT "         TEST RESULTS SUMMARY"
4050 PRINT "========================================="
4060 PRINT
4070 PRINT "Tests passed: "; TP%
4080 PRINT "Tests failed: "; TF%
4090 PRINT
4100 IF TF% = 0 THEN PRINT "ALL TESTS PASSED!"
4110 IF TF% > 0 THEN PRINT "SOME TESTS FAILED"
4120 PRINT
4130 PRINT "Press any key to cleanup test files..."
4140 GETKEY K$
4150 REM =============================================
4160 REM  Cleanup test files
4170 REM =============================================
4180 PRINT "Cleaning up test files..."
4190 SCRATCH TESTFILE$
4200 SCRATCH "file1.txt"
4210 SCRATCH "file2.txt"
4220 SCRATCH "cmdtest.txt"
4230 SCRATCH "alias.txt"
4240 SCRATCH "append.txt"
4250 SCRATCH "dclear1.txt"
4260 SCRATCH "dclear2.txt"
4270 SCRATCH "dclear3.txt"
4280 SCRATCH "record.txt"
4290 PRINT "Test files removed"
4300 END

9500 REM *** Assert numeric equality ***
9510 IF EX = AC THEN GOTO 9560
9520 PRINT "  [FAIL] "; TN$; " expected "; EX; " got "; AC
9530 TF% = TF% + 1
9540 RETURN
9560 PRINT "  [PASS] "; TN$
9570 TP% = TP% + 1
9580 RETURN

9700 PRINT: PRINT "Press any key to continue..."
9710 GETKEY K$
9720 PRINT "------------------------------------------": PRINT
9730 RETURN
