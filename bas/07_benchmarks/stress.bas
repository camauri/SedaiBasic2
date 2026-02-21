1 rem ================================
2 rem   tiny basic ssa / bytecode test
3 rem ================================

10 rem ----- constants -----
20 pi = 3.14159265358979
30 x0 = 1.2345
40 f0 = 0.0
50 s0 = 0

100 rem ================================
110 rem test 1: integer for / step
120 rem ================================
130 s1 = 0
140 for i = 1 to 10
150 s1 = s1 + i
160 next i
170 rem expected: 55
180 print "s1=";s1:if s1 = 55 then g1 = 1 : else g1 = 0

200 rem ================================
210 rem test 2: float arithmetic
220 rem ================================
230 f1 = (pi * 2.0) + 1.0
240 rem expected: 7.28318530717958
250 rem reduce precision for check
260 if f1 > 7.28318 then if f1 < 7.28319 then g2 = 1 : else g2 = 0
270 print "f1=";f1:if f1 <= 7.28318 then g2 = 0

300 rem ================================
310 rem test 3: array 1d
320 rem ================================
330 dim a(5)
340 a(0) = 10
350 a(1) = 20
360 a(2) = 30
370 a(3) = 40
380 a(4) = 50
390 t1 = a(0)+a(1)+a(2)+a(3)+a(4)
400 rem expected: 150
410 print "t1=";t1:if t1 = 150 then g3 = 1 : else g3 = 0

500 rem ================================
510 rem test 4: 2d array + for nested
520 rem ================================
530 dim b(3,3)
540 c1 = 0
550 for i = 0 to 2
560 for j = 0 to 2
570 b(i,j) = (i+1)*(j+2)
580 c1 = c1 + b(i,j)
590 next j
600 next i
610 rem expected sum:
620 rem row 0: 1*2 + 1*3 + 1*4 =  9
630 rem row 1: 2*2 + 2*3 + 2*4 = 18
640 rem row 2: 3*2 + 3*3 + 3*4 = 27
650 rem total = 54
660 print "c1=";c1:if c1 = 54 then g4 = 1 : else g4 = 0

700 rem ================================
710 rem test 5: function via gosub
720 rem ================================
730 x1 = 0
740 y1 = 5.5
750 gosub 9000
760 rem f returned in r1
770 rem expected: x1*x1 + y1*2 = 0 + 11 = 11
780 print "r1=";r1:if r1 = 11 then g5 = 1 : else g5 = 0

800 rem ================================
810 rem test 6: algebraic simplification
820 rem ================================
830 rem test x + 0 = x (int identity)
840 n1 = 42
850 n2 = n1 + 0
860 if n2 = 42 then a1 = 1 : else a1 = 0
870
880 rem test 0 + x = x (int identity commutative)
890 n3 = 0 + n1
900 if n3 = 42 then a2 = 1 : else a2 = 0
910
920 rem test x * 1 = x (int identity)
930 n4 = n1 * 1
940 if n4 = 42 then a3 = 1 : else a3 = 0
950
960 rem test 1 * x = x (int identity commutative)
970 n5 = 1 * n1
980 if n5 = 42 then a4 = 1 : else a4 = 0
990
1000 rem test x * 0 = 0 (int annihilation)
1010 n6 = n1 * 0
1020 if n6 = 0 then a5 = 1 : else a5 = 0
1030
1040 rem test 0 * x = 0 (int annihilation commutative)
1050 n7 = 0 * n1
1060 if n7 = 0 then a6 = 1 : else a6 = 0
1070
1080 rem test x - 0 = x (int identity)
1090 n8 = n1 - 0
1100 if n8 = 42 then a7 = 1 : else a7 = 0
1110
1120 rem test x / 1 = x (int identity)
1130 n9 = n1 / 1
1140 if n9 = 42 then a8 = 1 : else a8 = 0
1150
1160 rem test x - x = 0 (int inverse)
1170 n10 = n1 - n1
1180 if n10 = 0 then a9 = 1 : else a9 = 0
1190
1200 rem test x / x = 1 (int inverse)
1210 n11 = n1 / n1
1220 if n11 = 1 then a10 = 1 : else a10 = 0
1230
1240 rem ===== float algebraic tests =====
1250 rem test x + 0.0 = x (float identity)
1260 f2 = 3.14
1270 f3 = f2 + 0.0
1280 if f3 > 3.13 then if f3 < 3.15 then a11 = 1 : else a11 = 0
1290 if f3 <= 3.13 then a11 = 0
1300
1310 rem test x * 1.0 = x (float identity)
1320 f4 = f2 * 1.0
1330 if f4 > 3.13 then if f4 < 3.15 then a12 = 1 : else a12 = 0
1340 if f4 <= 3.13 then a12 = 0
1350
1360 rem test x * 0.0 = 0.0 (float annihilation)
1370 f5 = f2 * 0.0
1380 if f5 > -0.001 then if f5 < 0.001 then a13 = 1 : else a13 = 0
1390 if f5 <= -0.001 then a13 = 0
1400
1410 rem test x - 0.0 = x (float identity)
1420 f6 = f2 - 0.0
1430 if f6 > 3.13 then if f6 < 3.15 then a14 = 1 : else a14 = 0
1440 if f6 <= 3.13 then a14 = 0
1450
1460 rem test x / 1.0 = x (float identity)
1470 f7 = f2 / 1.0
1480 if f7 > 3.13 then if f7 < 3.15 then a15 = 1 : else a15 = 0
1490 if f7 <= 3.13 then a15 = 0
1500
1510 rem aggregate algebraic test result
1520 g6 = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15
1530 rem expected: all 15 sub-tests pass = 15
1540 print "g6=";g6:if g6 = 15 then g6 = 1 : else g6 = 0

1600 rem ================================
1610 rem test 7: strength reduction
1620 rem ================================
1630 rem test x * 2 → x + x (int strength reduction)
1640 s1 = 100
1650 s2 = s1 * 2
1660 rem expected: 200
1670 if s2 = 200 then sr1 = 1 : else sr1 = 0
1680
1690 rem test x * 2 → x + x (float strength reduction)
1700 sf1 = 50.5
1710 sf2 = sf1 * 2.0
1720 rem expected: 101.0
1730 if sf2 > 100.9 then if sf2 < 101.1 then sr2 = 1 : else sr2 = 0
1740 if sf2 <= 100.9 then sr2 = 0
1750
1760 rem test x * 4 (would become x << 2 with shift support)
1770 s3 = 25
1780 s4 = s3 * 4
1790 rem expected: 100
1800 if s4 = 100 then sr3 = 1 : else sr3 = 0
1810
1820 rem test x * 8 (would become x << 3 with shift support)
1830 s5 = 12
1840 s6 = s5 * 8
1850 rem expected: 96
1860 if s6 = 96 then sr4 = 1 : else sr4 = 0
1870
1880 rem test combined: (x * 2) + (y * 2)
1890 s7 = 30
1900 s8 = 20
1910 s9 = (s7 * 2) + (s8 * 2)
1920 rem expected: 60 + 40 = 100
1930 if s9 = 100 then sr5 = 1 : else sr5 = 0
1940
1950 rem aggregate strength reduction test result
1960 g7 = sr1 + sr2 + sr3 + sr4 + sr5
1970 rem expected: all 5 sub-tests pass = 5
1980 print "g7=";g7:if g7 = 5 then g7 = 1 : else g7 = 0

2000 rem ================================
2010 rem test 8: const keyword
2020 rem ================================
2030 rem test basic const assignment
2040 const c1 = 42
2050 const c2 = 3.14159
2060 c3 = c1 + 10
2070 rem expected: c3 = 52
2080 if c3 = 52 then t1 = 1 : else t1 = 0
2090
2100 rem test const in expression
2110 c4 = c1 * 2
2120 rem expected: c4 = 84
2130 if c4 = 84 then t2 = 1 : else t2 = 0
2140
2150 rem test float const
2160 c5 = c2 * 2.0
2170 rem expected: c5 ≈ 6.28318
2180 if c5 > 6.28 then if c5 < 6.29 then t3 = 1 : else t3 = 0
2190 if c5 <= 6.28 then t3 = 0
2200
2210 rem aggregate const test result
2220 g8 = t1 + t2 + t3
2230 rem expected: all 3 sub-tests pass = 3
2240 print "g8=";g8:if g8 = 3 then g8 = 1 : else g8 = 0

8000 rem ================================
8010 rem final report
8020 rem ================================

8030 print "TEST RESULTS"
8040 print "t1 (int for/step): ";: if g1 = 1 then print "OK" : else print "FAIL": PRINT " g1="; g1
8050 print "t2 (float arith) : ";: if g2 = 1 then print "OK" : else print "FAIL": PRINT " g2="; g2
8060 print "t3 (array 1d)    : ";: if g3 = 1 then print "OK" : else print "FAIL": PRINT " g3="; g3
8070 print "t4 (array 2d)    : ";: if g4 = 1 then print "OK" : else print "FAIL": PRINT " g4="; g4
8080 print "t5 (gosub)       : ";: if g5 = 1 then print "OK" : else print "FAIL": PRINT " g5="; g5
8085 print "t6 (algebraic)   : ";: if g6 = 1 then print "OK" : else print "FAIL": PRINT " g6="; g6
8087 print "t7 (strength)    : ";: if g7 = 1 then print "OK" : else print "FAIL": PRINT " g7="; g7
8089 print "t8 (const)       : ";: if g8 = 1 then print "OK" : else print "FAIL": PRINT " g8="; g8

8090 end

9000 rem --- function: r1 = x1*x1 + y1*2 ---
9010 r1 = x1*x1
9020 r1 = r1 + (y1*2.0)
9030 return