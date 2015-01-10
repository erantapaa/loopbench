
all: h5 c5

h5: h5.hs
	ghc -O2 h5.hs -rtsopts

c5: c5.c
	gcc -std=c11 -lm -O2 c5.c -o c5


