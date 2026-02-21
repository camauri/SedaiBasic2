10 a = rgr(0)
20 color 2,0: color 3,1
20 graphic 1
30 scnclr: rem goto 500
100 box 1,30,30,80,80,60,1
110 box 2,50,30,100,80,30,1
120 box 3,70,30,120,80,15,1
130 box 1,90,30,140,80,7.5,1
140 box 2,110,30,160,80,3.75,1
150 box 3,130,30,180,80,1.875,1
160 box 1,150,30,200,80,0.9375,1
170 box 2,170,30,220,80,0.4688,1
180 box 3,190,30,240,80,0.2344,1
190 rem end
200 width 4: circle 1,50,130,25,25
210 width 2: circle 2,70,130,25,25
220 width 1: circle 3,90,130,25,25
230 width 4: circle 1,150,130,25,25,0,120
240 width 2: circle 2,170,130,25,25,0,240
250 width 1: circle 3,190,130,25,25,0,360
260 sleep 5: scnclr
300 circle 1,60,40,20,18,,,,45: REM octagon (45° step)
310 circle 1,260,40,20,30,,,,90: REM diamond (90° step)
320 circle 1,60,140,20,18,,,,120: REM triangle (120° step)
330 sleep 1
340 paint 1,60,40
350 sleep 3
360 paint 2,260,40,1
370 sleep 5: scnclr
400 draw 1, 250, 20 to 50, 20 to 50, 90 to 250, 20
410 locate 50,90
420 print "CHECK AFTER LOCATE(50,90)"
430 print "x=";rdot(0)
440 print "y=";rdot(1)
450 print "color=";rdot(2)
460 sleep 3: scnclr
500 circle 1,50,50,20
510 sshape s$,20,20,80,80
520 sleep 3 
530 print "CHECK AFTER SSHAPE s$,20,20,80,80" 
540 print "sshape len=";len(s$)
550 gshape s$,150,100
560 sleep 3
570 scale 1,640,400: circle 1,70,70,20
580 rem sleep 5
590 scale 0
1000 sleep 3
1010 graphic a
