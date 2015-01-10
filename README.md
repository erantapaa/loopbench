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

