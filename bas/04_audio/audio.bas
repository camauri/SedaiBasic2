10 rem =============================================
20 rem  SedaiBasic Audio Test Suite (Interactive)
30 rem  Tests: VOL, ENVELOPE, TEMPO, PLAY, SOUND, FILTER
40 rem  Y=passed, N=failed, R=repeat test
50 rem =============================================
60 print chr$(147)
70 print "=== SEDAIBASIC AUDIO TEST SUITE ==="
80 print "After each test, press:"
90 print "  Y = Test passed (heard expected sound)"
100 print "  N = Test failed (no sound or wrong)"
110 print "  R = Repeat test"
120 print
130 passed% = 0 : failed% = 0 : total% = 0

200 rem =============================================
210 rem  TEST 1: VOL + Basic PLAY
220 rem =============================================
230 total% = total% + 1
240 print "TEST 1: VOL + Basic PLAY"
250 print "Expected: Musical scale C-D-E-F-G-A-B"
260 print "          7 distinct notes, rising pitch"
270 print
280 gosub 5000 : rem wait for keypress to start
290 vol 8
300 play "cdefgab"
310 print
320 print "Did you hear 7 rising notes (do-re-mi...)?"
330 gosub 5100 : rem get Y/N/R response
340 if r$ = "R" then 240
350 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
360 print

400 rem =============================================
410 rem  TEST 2: TEMPO - Fast vs Slow
420 rem =============================================
430 total% = total% + 1
440 print "TEST 2: TEMPO command"
450 print "Expected: First scale FAST, second scale SLOW"
460 print "          Same notes, different speeds"
470 print
480 gosub 5000
490 print "  Playing FAST (tempo 20)..."
500 tempo 20
510 play "cdefgab"
520 print "  Playing SLOW (tempo 4)..."
530 tempo 4
540 play "cdefgab"
550 tempo 8
560 print
570 print "Did you hear FAST scale then SLOW scale?"
580 gosub 5100
590 if r$ = "R" then 440
600 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
610 print

700 rem =============================================
710 rem  TEST 3: PLAY - Octaves (pitch difference)
720 rem =============================================
730 total% = total% + 1
740 print "TEST 3: PLAY - Octave Selection"
750 print "Expected: 3 scales at different pitches"
760 print "          LOW (deep) -> MIDDLE -> HIGH (shrill)"
770 print
780 gosub 5000
790 tempo 12
800 print "  Octave 2 (LOW, deep bass)..."
810 play "o2 cdefg"
820 print "  Octave 4 (MIDDLE, normal)..."
830 play "o4 cdefg"
840 print "  Octave 6 (HIGH, shrill)..."
850 play "o6 cdefg"
860 print
870 print "Did you hear 3 scales: LOW -> MIDDLE -> HIGH?"
880 gosub 5100
890 if r$ = "R" then 740
900 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
910 print

1000 rem =============================================
1010 rem  TEST 4: PLAY - Note Durations
1020 rem =============================================
1030 total% = total% + 1
1040 print "TEST 4: PLAY - Note Durations"
1050 print "Expected: Notes of DECREASING length"
1060 print "          LONG C, shorter D, shorter E..."
1070 print
1080 gosub 5000
1090 tempo 8
1100 print "  W=whole(long), H=half, Q=quarter, I=8th, S=16th(short)"
1110 play "wc hd qe if sg"
1120 print
1130 print "Did notes get progressively SHORTER?"
1140 gosub 5100
1150 if r$ = "R" then 1040
1160 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
1170 print

1200 rem =============================================
1210 rem  TEST 5: PLAY - Voices (3 independent channels)
1220 rem =============================================
1230 total% = total% + 1
1240 print "TEST 5: PLAY - Voice Selection"
1250 print "Expected: 3 separate scales"
1260 print "          Each voice should sound similar"
1270 print
1280 gosub 5000
1290 tempo 12
1300 print "  Voice 1..."
1310 play "v1 o4 cdefg"
1320 print "  Voice 2..."
1330 play "v2 o4 cdefg"
1340 print "  Voice 3..."
1350 play "v3 o4 cdefg"
1360 print
1370 print "Did you hear 3 similar scales (V1, V2, V3)?"
1380 gosub 5100
1390 if r$ = "R" then 1240
1400 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
1410 print

1500 rem =============================================
1510 rem  TEST 6: ENVELOPE - Custom instrument
1520 rem =============================================
1530 total% = total% + 1
1540 print "TEST 6: ENVELOPE - Piano vs Organ"
1550 print "Expected: First=piano (notes fade out)"
1560 print "          Second=organ (notes sustain fully)"
1570 print
1580 gosub 5000
1590 tempo 8
1600 print "  Piano (A=0 D=9 S=0 R=9 - fading notes)..."
1610 envelope 0,0,9,0,9,1
1620 play "t0 cdefg"
1630 print "  Organ (A=0 D=0 S=15 R=0 - sustained notes)..."
1640 envelope 1,0,0,15,0,1
1650 play "t1 cdefg"
1660 print
1670 print "Did piano FADE while organ SUSTAINED?"
1680 gosub 5100
1690 if r$ = "R" then 1540
1700 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
1710 print

1800 rem =============================================
1810 rem  TEST 7: PLAY - Volume control (Un)
1820 rem =============================================
1830 total% = total% + 1
1840 print "TEST 7: PLAY - Volume Control (Un)"
1850 print "Expected: 3 scales at increasing volume"
1860 print "          SOFT -> MEDIUM -> LOUD"
1870 print
1880 gosub 5000
1890 tempo 12
1900 print "  U2 (very soft)..."
1910 play "u2 cdefg"
1920 print "  U5 (medium)..."
1930 play "u5 cdefg"
1940 print "  U9 (loud)..."
1950 play "u9 cdefg"
1960 print
1970 print "Did volume increase: SOFT -> MEDIUM -> LOUD?"
1980 gosub 5100
1990 if r$ = "R" then 1840
2000 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
2010 print

2100 rem =============================================
2110 rem  TEST 8: SOUND - Basic tone
2120 rem =============================================
2130 total% = total% + 1
2140 print "TEST 8: SOUND - Basic Tones"
2150 print "Expected: 3 beeps at different pitches"
2160 print "          LOW beep -> MEDIUM beep -> HIGH beep"
2170 print
2175 vol 15
2180 gosub 5000
2190 print "  Low frequency (4096)..."
2200 sound 1,4096,60
2210 print "  Medium frequency (8192)..."
2220 sound 1,8192,60
2230 print "  High frequency (16384)..."
2240 sound 1,16384,60
2250 print
2260 print "Did you hear 3 beeps: LOW -> MEDIUM -> HIGH?"
2270 gosub 5100
2280 if r$ = "R" then 2140
2290 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
2300 print

2400 rem =============================================
2410 rem  TEST 9: SOUND - Waveforms
2420 rem =============================================
2430 total% = total% + 1
2440 print "TEST 9: SOUND - Waveforms"
2450 print "Expected: 4 DIFFERENT timbres"
2460 print "  1=Noise, 2=Pulse, 3=Triangle, 4=Sawtooth"
2470 print
2480 gosub 5000
2490 print "  1. Noise (static hiss)..."
2500 sound 1,8192,90,0,0,0,3,0
2510 print "  2. Pulse 50% (hollow square)..."
2520 sound 1,8192,90,0,0,0,2,2048
2530 print "  3. Triangle (smooth mellow)..."
2540 sound 1,8192,90,0,0,0,0,0
2550 print "  4. Sawtooth (bright buzzy)..."
2560 sound 1,8192,90,0,0,0,1,0
2570 print
2580 print "Which sounds did you hear (1/2/3/4)?"
2590 gosub 5100
2600 if r$ = "R" then 2440
2610 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
2620 print

2700 rem =============================================
2710 rem  TEST 10: SOUND - Frequency Sweep
2720 rem =============================================
2730 total% = total% + 1
2740 print "TEST 10: SOUND - Frequency Sweep"
2750 print "Expected: Rising pitch (like rocket launch)"
2760 print
2770 gosub 5000
2780 print "  Sweeping UP from low to high..."
2790 sound 1,0,180,0,0,300,1,0
2800 print
2810 print "Did pitch rise continuously (whoooosh up)?"
2820 gosub 5100
2830 if r$ = "R" then 2740
2840 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
2850 print

2900 rem =============================================
2910 rem  TEST 11: SOUND - Sweep Down
2920 rem =============================================
2930 total% = total% + 1
2940 print "TEST 11: SOUND - Sweep Down"
2950 print "Expected: Falling pitch (like bomb drop)"
2960 print
2970 gosub 5000
2980 print "  Sweeping DOWN from high to low..."
2990 sound 1,49152,180,1,0,300,1,0
3000 print
3010 print "Did pitch fall continuously (bomb drop)?"
3020 gosub 5100
3030 if r$ = "R" then 2940
3040 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
3050 print

3100 rem =============================================
3110 rem  TEST 12: SOUND - Oscillate (siren)
3120 rem =============================================
3130 total% = total% + 1
3140 print "TEST 12: SOUND - Oscillating Sweep"
3150 print "Expected: Siren effect (up and down)"
3160 print
3170 gosub 5000
3180 print "  Oscillating (siren)..."
3190 sound 1,49152,240,2,20000,2000,1,0
3200 print
3210 print "Did pitch go UP and DOWN (wee-woo siren)?"
3220 gosub 5100
3230 if r$ = "R" then 3140
3240 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
3250 print

3300 rem =============================================
3310 rem  TEST 13: FILTER - Low-pass
3320 rem =============================================
3330 total% = total% + 1
3340 print "TEST 13: FILTER - Low-pass"
3350 print "Expected: First=bright, Second=muffled/dull"
3360 print "          Low-pass removes high frequencies"
3370 print
3380 gosub 5000
3390 tempo 10
3400 print "  WITHOUT filter (bright, clear)..."
3410 play "v1 o4 t0 u8 x0 cdefgab"
3420 print "  WITH low-pass filter (muffled, dull)..."
3430 filter 1000,1,0,0,0
3440 play "v1 o4 t0 u8 x1 cdefgab"
3450 print
3460 print "Did second scale sound MORE MUFFLED?"
3470 gosub 5100
3480 if r$ = "R" then 3340
3490 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
3500 print

3600 rem =============================================
3610 rem  TEST 14: FILTER - High-pass
3620 rem =============================================
3630 total% = total% + 1
3640 print "TEST 14: FILTER - High-pass"
3650 print "Expected: First=full, Second=thin/tinny"
3660 print "          High-pass removes bass frequencies"
3670 print
3680 gosub 5000
3690 tempo 10
3700 print "  WITHOUT filter (full sound)..."
3710 play "v1 o3 t0 u8 x0 cdefgab"
3720 print "  WITH high-pass filter (thin, tinny)..."
3730 filter 8000,0,0,1,0
3740 play "v1 o3 t0 u8 x1 cdefgab"
3750 print
3760 print "Did second scale sound THINNER/TINNY?"
3770 gosub 5100
3780 if r$ = "R" then 3640
3790 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
3800 print

3900 rem =============================================
3910 rem  TEST 15: PLAY - Rests
3920 rem =============================================
3930 total% = total% + 1
3940 print "TEST 15: PLAY - Rests (silence between notes)"
3950 print "Expected: C - pause - D - pause - E"
3960 print "          Clear gaps of silence between notes"
3970 print
3980 gosub 5000
3990 tempo 8
4000 print "  Playing C - rest - D - rest - E..."
4010 play "qc r qd r qe"
4020 print
4030 print "Did you hear gaps of SILENCE between notes?"
4040 gosub 5100
4050 if r$ = "R" then 3940
4060 if r$ = "Y" then passed% = passed% + 1 : else failed% = failed% + 1
4070 print

4200 rem =============================================
4210 rem  FINAL RESULTS
4220 rem =============================================
4230 print chr$(147)
4240 print "========================================="
4250 print "         TEST RESULTS SUMMARY"
4260 print "========================================="
4270 print
4280 print "Total tests: "; total%
4290 print "Passed:      "; passed%;
4300 if total% > 0 then pct% = int(passed%*100/total%) : else pct% = 0
4310 print " ("; pct%; "%)"
4320 print "Failed:      "; failed%;
4330 if total% > 0 then pct% = int(failed%*100/total%) : else pct% = 0
4340 print " ("; pct%; "%)"
4350 print
4360 if failed% = 0 then print "ALL TESTS PASSED!" : else print "Some tests failed - check audio implementation"
4370 print
4380 print "========================================="
4390 end

5000 rem =============================================
5010 rem  SUBROUTINE: Wait for keypress to start test
5020 rem =============================================
5030 print "Press any key to start test..."
5040 getkey k$
5050 return

5100 rem =============================================
5110 rem  SUBROUTINE: Get Y/N/R response
5120 rem =============================================
5130 print "(y=yes, n=no, r=repeat): ";
5140 getkey r$
5150 r$ = chr$(asc(r$) and 223) : rem uppercase
5160 if r$ <> "Y" and r$ <> "N" and r$ <> "R" then 5140
5170 print r$
5180 return
