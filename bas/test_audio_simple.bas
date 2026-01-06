10 rem Simple audio test with VOL
20 print "Testing VOL command (0-15 range)..."
30 print "VOL 15 (max volume) - should hear note"
40 vol 15
50 play "c"
60 print "VOL 0 (silence) - should NOT hear note"
70 vol 0
80 play "c"
90 print "VOL 8 (mid volume) - should hear quieter note"
100 vol 8
110 play "c"
120 print "Done"
130 end
