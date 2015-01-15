
.PHONY: run-haskell run-c

all: h5 c5

h5: h5.hs
	ghc -O2 h5.hs -rtsopts

c5: c5.c
	gcc -std=c11 -lm -O2 c5.c -o c5

run-haskell:
	for x in 1 2 3 4 5 6 7 8 9 10; do runhaskell ./run.hs h 10 20; done

run-c:
	for x in 1 2 3 4 5 6 7 8 9 10; do runhaskell ./run.hs c 10 20; done

