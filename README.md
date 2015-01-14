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

     e   ctime   htime  allocated  gc-bytes  alloc/iter       h/c       dps
    10  0.0099   0.010      78256      3408   76.421875  1.010101  0.046566
    11  0.0149   0.020      94640      3408   46.210938  1.342282  1.187436
    12  0.0258   0.042     127408      3408   31.105469  1.627907  1.885928
    13  0.0437   0.090     192944      3408   23.552734  2.059497  2.695015
    14  0.0818   0.173     324056      3408   19.778809  2.114914  2.654269
    15  0.1596   0.342     586200      9600   17.889404  2.142857  2.654269
    16  0.3145   0.686    1110488     12096   16.944702  2.181240  2.703018
    17  0.6116   1.376    2159104     15920   16.472656  2.249836  2.780871
    18  1.2302   2.759    4256256     15952   16.236328  2.242725  2.780871
    19  2.4688   5.519    8450560     16016   16.118164  2.235499  2.774141
    20  4.9758  11.042   16839208     16144   16.059120  2.219141  2.758588

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
