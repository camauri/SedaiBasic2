10 rem test: play with string variable vs literal
20 print "test 1: play literal..."
25 rem vol 8
30 play "cdefg"
40 print "test 1 done"
50 print "test 2: play variable..."
60 a$="cdefg"
70 play a$
80 print "test 2 done"
90 print "test 3: spaceless mode play variable..."
100 option spaceless
110 b$="cdefg"
120 play b$
130 print"test 3 done"
140 print"all tests complete"
