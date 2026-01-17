10 rem analog clock v1.0
20 rem (c) by marek karcz 2014
30 rem all rights reserved
40 rem
50 rem variables
60 hr%=0:mi%=0:se%=0:pr%=0:hm%=0
70 hr$="":mi$="":se$="":hm$=""
80 input "set time (hhmmss) ";tm$
90 ti$=tm$
900 goto 10000
1000 rem initgr
1005 scnclr
1010 graphic 1,1:width 2
1020 color 0,1:color 1,6:color 5,6
1030 circle 1,160,100,110,90
1040 return
1050 rem draw short hand
1060 draw 1,160,100 to 50,hr%*(360/12)+(mi%/15)*(360/48)
1070 return
1080 rem draw long hand
1090 draw 1,160,100 to 90,mi%*(360/60)
1110 return
1130 rem eraseshorthand
1140 locate 160,100
1150 draw 0,160,100 to 50,hr%*(360/12)+(mi%/15)*(360/48)
1160 return
1170 rem eraselonghand
1180 draw 0,160,100 to 90,mi%*(360/60)
1190 return
1200 rem gethr
1210 hr$=mid$(ti$,1,2)
1220 hr%=val(hr$)
1225 if hr%>11 then hr%=hr%-12
1230 return
1240 rem getmi
1250 mi$=mid$(ti$,3,2)
1260 mi%=val(mi$)
1270 return
1280 rem getsec
1290 se$=mid$(ti$,5,2)
1300 se%=val(se$)
1310 return
1320 rem draw seconds
1330 char 1,0,24,se$,1
1340 return
1350 rem gethrmi
1360 hm$=mid$(ti$,1,4)
1370 hm%=val(hm$)
1380 return
10000 rem main
10010 gosub 1000
10015 gosub 1200:gosub 1240:gosub 1280:rem get hr%,mi%,se%
10017 rem gosub 1050:gosub 1090:rem draw hands
10020 do
10025 gosub 1350:get hrmi
10030 if pr%<>hm% then begin:
10040 gosub 1130:gosub 1170:rem erase hands
10050 gosub 1200:gosub 1240:rem get hr%,mi%
10060 gosub 1050:gosub 1090:rem draw hands
10070 bend:
10080 gosub 1280:rem get se%
10090 gosub 1320:rem draw seconds
10100 pr%=hm%
10110 loop:rem loop forever
20000 end