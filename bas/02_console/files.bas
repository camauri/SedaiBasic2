10 REM =============================================
20 REM  SedaiBasic File I/O Test Suite
30 REM  Tests: DOPEN, DCLOSE, PRINT#, INPUT#, GET#, CMD
40 REM =============================================
50 PRINT CHR$(147)
60 PRINT "=== FILE I/O TEST SUITE ==="
70 PRINT
80 TP% = 0 : TF% = 0
90 TESTFILE$ = "testfile.txt"

100 REM =============================================
110 REM  TEST 1: Write to File
120 REM =============================================
130 PRINT "TEST 1: Write to File (DOPEN/PRINT#)"
140 PRINT "------------------------------"
150 DOPEN #1, TESTFILE$, "W"
160 PRINT# 1, "Hello from SedaiBasic!"
170 PRINT# 1, "Line 2: Numbers follow"
180 PRINT# 1, 100
190 PRINT# 1, 3.14159
200 PRINT# 1, "Final line"
210 DCLOSE #1
220 PRINT "File written successfully"
230 TP% = TP% + 1
240 PRINT

300 REM =============================================
310 REM  TEST 2: Read from File (INPUT#)
320 REM =============================================
330 PRINT "TEST 2: Read from File (INPUT#)"
340 PRINT "------------------------------"
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
480 PRINT

600 REM =============================================
610 REM  TEST 3: GET# (Character by character)
620 REM =============================================
630 PRINT "TEST 3: Read Character by Character (GET#)"
640 PRINT "------------------------------"
650 DOPEN #1, TESTFILE$, "R"
660 PRINT "First 20 characters: ";
670 FOR I = 1 TO 20
680 GET# 1, C$
690 IF C$ <> "" THEN PRINT C$;
700 NEXT I
710 DCLOSE #1
720 PRINT
730 TP% = TP% + 1
740 PRINT

800 REM =============================================
810 REM  TEST 4: Append Mode
820 REM =============================================
830 PRINT "TEST 4: Append Mode"
840 PRINT "------------------------------"
850 DOPEN #1, TESTFILE$, "A"
860 PRINT# 1, "Appended line 1"
870 PRINT# 1, "Appended line 2"
880 DCLOSE #1
890 PRINT "Lines appended"
900 REM Verify
910 DOPEN #1, TESTFILE$, "R"
920 LC% = 0
930 ON ERROR GOTO 970
940 INPUT# 1, DUMMY$
950 LC% = LC% + 1
960 GOTO 940
970 DCLOSE #1
980 PRINT "Total lines in file: "; LC%
990 TN$ = "Line count after append": EX = 7: AC = LC%: GOSUB 9500
1000 PRINT

1100 REM =============================================
1110 REM  TEST 5: Multiple File Handles
1120 REM =============================================
1130 PRINT "TEST 5: Multiple File Handles"
1140 PRINT "------------------------------"
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
1320 PRINT

1400 REM =============================================
1410 REM  TEST 6: CMD Redirection
1420 REM =============================================
1430 PRINT "TEST 6: CMD Output Redirection"
1440 PRINT "------------------------------"
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
1590 PRINT

1700 REM =============================================
1710 REM  TEST 7: OPEN/CLOSE Aliases
1720 REM =============================================
1730 PRINT "TEST 7: OPEN/CLOSE Aliases"
1740 PRINT "------------------------------"
1750 OPEN #1, "alias.txt", "W"
1760 PRINT# 1, "Testing OPEN alias"
1770 CLOSE #1
1780 OPEN #1, "alias.txt", "R"
1790 INPUT# 1, AL$
1800 CLOSE #1
1810 PRINT "Read: "; AL$
1820 IF INSTR(AL$, "OPEN") > 0 THEN TP% = TP% + 1: PRINT "  [PASS] OPEN/CLOSE aliases"
1830 IF INSTR(AL$, "OPEN") = 0 THEN TF% = TF% + 1: PRINT "  [FAIL] OPEN/CLOSE aliases"
1840 PRINT

1900 REM =============================================
1910 REM  Cleanup test files
1920 REM =============================================
1930 PRINT "Cleaning up test files..."
1940 SCRATCH TESTFILE$
1950 SCRATCH "file1.txt"
1960 SCRATCH "file2.txt"
1970 SCRATCH "cmdtest.txt"
1980 SCRATCH "alias.txt"
1990 PRINT "Test files removed"
2000 PRINT

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
3130 END

9500 REM *** Assert numeric equality ***
9510 IF EX = AC THEN GOTO 9560
9520 PRINT "  [FAIL] "; TN$; " expected "; EX; " got "; AC
9530 TF% = TF% + 1
9540 RETURN
9560 PRINT "  [PASS] "; TN$
9570 TP% = TP% + 1
9580 RETURN
