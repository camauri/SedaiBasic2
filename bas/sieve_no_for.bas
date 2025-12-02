10 REM Sieve of Eratosthenes - Find all primes up to N
20 PRINT "sieve of eratosthenes benchmark"
30 N = 9999
40 DIM FLAGS(10000)
50 PRINT "please wait, finding primes up to 10000... "
60 REM Initialize all flags to 1 (assume all numbers are prime)
70 I = 0
80 IF I > N THEN GOTO 120
90 FLAGS(I) = 1
100 I = I + 1
110 GOTO 80
120 REM Sieve: mark multiples of each prime as composite
130 I = 2
140 IF I * I > N THEN GOTO 200
150 IF FLAGS(I) = 0 THEN GOTO 190
160 J = I * I
170 IF J > N THEN GOTO 190
180 FLAGS(J) = 0
182 J = J + I
184 GOTO 170
190 I = I + 1
195 GOTO 140
200 REM Count primes
210 PC = 0
220 I = 2
230 IF I > N THEN GOTO 270
240 IF FLAGS(I) = 1 THEN PC = PC + 1
250 I = I + 1
260 GOTO 230
270 PRINT "found "; PC; " primes... ";
280 IF PC = 1229 THEN PRINT "correct!" ELSE PRINT "ERROR!"
290 END
