        PARAMETER (MAXN = 10000, RTMAXN = 100)
        INTEGER*1 PRIMES
        DIMENSION PRIMES(MAXN)

C       FORTRAN standard does not let us initialise data to zero 
C       automatically.
        DO 10 I=1, MAXN
        PRIMES(I) = 0
10      CONTINUE

        DO 30 I=2, RTMAXN
        IF (PRIMES(I).ne.0) GO TO 30
        DO 20 J=I*I, MAXN, I
        PRIMES(J)=1
20      CONTINUE
30      CONTINUE


