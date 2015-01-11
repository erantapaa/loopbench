# loopbench

This is a collection of files used to benchmark looping in Haskell.

- h5.hs - the Haskell prgram
- c5.c - the C program
- run.hs - helper to run the C and Haskell programs with a range of parameters
- analyze - python program to report the results (requires pandas)

# Usage

    make all
    mkdir data

    # perform 10 runs of the C program for each e parameter
    for x in 1 2 3 4 5 6 7 8 9 10; do runhaskell ./run.hs c 10 20; done

    # perform 10 runs of the Haskell program for each e parameter
    for x in 1 2 3 4 5 6 7 8 9 10; do runhaskell ./run.hs h 10 20; done

    python ./analyze

The report will look like this:

     e   ctime      htime  allocated  gc-bytes  alloc/iter       h/c       dps
    10  0.0101   0.020000      87424      3408   85.375000  1.980198  4.610047
    11  0.0151   0.034545     112000      3408   54.687500  2.287778  4.527498
    12  0.0263   0.070000     161152      3408   39.343750  2.661597  5.087350
    13  0.0472   0.134545     259456      3408   31.671875  2.850539  5.084175
    14  0.0819   0.270909     456200      3408   27.844238  3.307803  5.500889
    15  0.1575   0.538182     849416      9616   25.922119  3.417027  5.539650
    16  0.3112   1.090000    1635848     15960   24.961060  3.502571  5.666516
    17  0.6105   2.168182    3208848     15984   24.481567  3.551485  5.666813
    18  1.2167   4.353636    6354576     16032   24.240784  3.578233  5.706054
    19  2.4092   8.733636   12646032     16128   24.120392  3.625119  5.752041
    20  4.8332  17.410909   25229080     16320   24.060326  3.602356  5.719680

    e          = exponent parameter
    ctime      = running time of the C program
    htime      = running time of the Haskell program
    allocated  = bytes allocated in the heap (Haskell program)
    gc-bytes   = bytes copied during GC (Haskell program)
    alloc/iter = bytes allocated in the heap / 2^e
    h / c      = htime divided by ctime
    dps        = (htime - ctime) divided by the number of divisor tests made
                 in picoseconds

   # divisor tests made = 2^e * 2^21
