5 print: print "FIBONACCI CALCULATOR"
10 input "Starting number (0 or 1): "; a
15 if a < 0 or a > 1 then goto 10
20 input "How many numbers? "; n
30 if n < 1 then goto 20
40 f1 = a: f2 = 1: print a; ", ";: c = 1
50 if n = 1 then goto 110
60 for i = 2 to n
70 f3 = f1 + f2
80 print f3;: if i < n then print ", "; : c = c + 1
85 if c = 5 then print: c = 0
90 f1 = f2: f2 = f3
100 next i
110 if c > 0 then print
120 end
