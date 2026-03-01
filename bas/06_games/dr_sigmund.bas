1 OPTION SPACELESS
5 graphic0,1
10 rem dr. sigmund1
20 scnclr
30 tempo 8
40 play "v1o4t5u8x0":rem v1=guitar
50 play "v2o4t5u8x0":rem v2=guitar
60 a$="v1o5ses#d"
70 b$="v1o5ses#dses#dseo4sbo5sdo4sc"
80 c$="v1o4.iav2o2sao3sev1o4irv2o3sao4scsesrv1o4sa"
90 d$="v1o4.ibv2o1sco3ses#g.irv1o4ses#gsb"
100 e$="v1o5qav2o2sao3sesao4ev1o5ses#d"
110 f$="v1o4wav2o2wa"
120 play a$:play b$:play c$: play d$
130 play e$:play b$:play c$: play d$
140 play f$
150 color4,12:color0,16:d$=chr$(147):dimrx(22),ry(22)
160 printchr$(147);chr$(28);"hello, i am sandy, assistant to":print"dr. sigmund."
170 input"is this your first visit?";a$
180 ifleft$(a$,1)="n"then310
190 print:input"what is your name";b$
200 print:print"you are in luck, "b$",":print"dr. sigmund will see you now.":sleep3
210 printchr$(147);"please, make yourself comfortable on":print"the couch."
220 print:print"i see that you are a little bit":print"nervous, "b$". ";
230 print"try to relax.":sleep5
240 print:print"i have some pretty pictures to show you."
250 print"just let your mind go to rest. look at"
260 print"the pictures and pick from the list"
270 print"what they remind you of. if you think"
280 print"of something else,just enter that.":s=0:sleep5
290 sleep5:rem rorshack test
300 s=s+1:ifs=5then910
310 graphic2,1:color1,7
320 draw1,159,0to159,199to160,199to160,0
330 draw1,0,159to319,159
340 draw1,0,0to319,0
350 forx=1to20:rx(x)=int(rnd(1)*320):next:rx(0)=0
360 forx=1to22:ry(x)=int(rnd(1)*160):next:ry(0)=80
370 forx=20to22:rx(x)=int(rnd(1)*75)+75:next
380 printd$"ok, here is a picture..."
390 forx=1to10
400 draw1,rx(x-1),ry(x-1)torx(x),ry(x)
410 draw1,319-rx(x-1),ry(x-1)to319-rx(x) ,ry(x)
420 next:forx=10to12
430 a=rnd(1)*180
440 xr=rnd(1)*100:yr=rnd(1)*100
450 circle1,rx(x),ry(x),xr,yr,,,a,5
460 circle1,319-rx(x),ry(x),xr,yr,,,360-a,5
470 next:forx=13to22
475 getkey k$
480 paint1,rx(x),ry(x)
485 getkey k$
490 paint1,319-rx(x),ry(x):next
495 getkey k$
500 printd$" spider   mother  cat    butterfly"
510 print" snake    camel   rope   reindeer"
520 print" father   face    food   ink-blot"
530 o$ = "":input o$:print d$;
540 ifo$="spider"then690
550 ifo$="mother"then710
560 ifo$="cat"then730
570 ifo$="snake"then750
580 ifo$="butterfly"then770
590 ifo$="rope"then790
600 ifo$="reindeer"then810
610 ifo$="food"then830
620 ifo$="father"then840
630 ifo$="ink-blot"then850
640 ifo$="face"then870
650 ifo$="camel"then890
660 print"tell me about your childhood.":inputp$
670 printd$:input"go on...";p$:printd$;:input"hmmm...tell me more.";p$
680 printd$"that is enough for now. call me tomorrow about this.":goto290
690 print"obviously you feel entangled. the"
700 print"spider is a symbol of your fear of":print"frustration.":goto290
710 print"your deep feelings for your mother are"
720 print"only natural. do not be afraid to let":print"them show.":goto290
730 print"somewhere in your sub-conscious you":print"have a fear of cats..."
740 print"you startle when one crosses your path":print"at night.":goto290
750 print"beware the snake. you need to control"
760 print"yourself in tense situations.":goto290
770 print"the butterfly symbolizes your free"
780 print"spirit and great inner joy. let":print"yourself have fun.":goto290
790 print"the rope is a sign of the strong support"
800 print"you offer your friends. you have great":print"inner reserves.":goto290
810 print"you are blessed with a childlike love"
820 print"for nature. your heart is pure.":goto290
830 print"either you are hungry or you desire to":print"feed others.":goto290
840 print"your feelings for your father must":print"someday be explored.":goto290
850 print"you are a realist. unfortunately, you"
860 print"are without imagination.":goto 290
870 print"you are like a newborn child; you are"
880 print"attracted to faces. you seek acceptance and love.":goto290
890 print"one hump or two?":print"seriously, you are alone in a spiritual"
900 print"desert. you can prepare to reenter the":print"world.":goto290
910 graphic0:printchr$(147);"thank you for coming."
920 print"i will see you again next week .":print"please leave $50 on the tv."