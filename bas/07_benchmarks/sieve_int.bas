10 rem sieve of eratosthenes benchmark - INTEGER VERSION
20 print "SIEVE OF ERATOSTHENES BENCHMARK (INTEGER)"
25 print "please wait, finding primes up to 10000... "
30 n% = 10000: ns% = 100: rem sqr(n%)
40 dim flags%(n%)
60 for i% = 2 to n%: flags%(i%) = 1: next i%
80 for i% = 2 to ns%
90   if flags%(i%) = 0 then goto 130
100   for j% = i% * i% to n% step i%: flags%(j%) = 0: next j%
130 next i%
150 pc% = 0
160 for i% = 2 to n%
170   if flags%(i%) = 1 then pc% = pc% + 1
180 next i%
190 print "found "; pc%; " primes... ";
200 if pc% = 1229 then print "correct!" else print "error!"
210 end
