# Copyright © 2013 Bart Massey
# [This work is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HS_SIEVES = massey-sieve oneill-sieve bird-sieve c2-sieve
SIEVES = $(HS_SIEVES) c-sieve

.SUFFIXES: .hs .hi

.hs.o:
	ghc -Wall -O2 $(GHCFLAGS) --make -c $*.hs

all: $(SIEVES)

massey-sieve: massey-sieve.o
	ghc -Wall -O2 --make massey-sieve

oneill-sieve: oneill-sieve.o PQ.o
	ghc -Wall -O2 --make oneill-sieve

bird-sieve: bird-sieve.o
	ghc -Wall -O2 --make bird-sieve

c2-sieve: c2-sieve.o
	ghc -Wall -O2 --make c2-sieve

c-sieve: c-sieve.c
	gcc -Wall -O2 -std=c99 -o c-sieve c-sieve.c

wheel: wheel.o
	ghc -Wall -O2 --make wheel

clean:
	-rm -f *.hi *.o $(SIEVES) wheel

$(HS_SIEVES): DefaultMain.o
DefaultMain.hi: DefaultMain.o
PQ.hi: PQ.o

# DO NOT DELETE: Beginning of Haskell dependencies
wheel.o : wheel.hs
PQ.o : PQ.hs
DefaultMain.o : DefaultMain.hs
bird-sieve.o : bird-sieve.hs
bird-sieve.o : DefaultMain.hi
c2-sieve.o : c2-sieve.hs
c2-sieve.o : DefaultMain.hi
massey-sieve.o : massey-sieve.hs
massey-sieve.o : DefaultMain.hi
oneill-sieve.o : oneill-sieve.hs
oneill-sieve.o : PQ.hi
oneill-sieve.o : DefaultMain.hi
# DO NOT DELETE: End of Haskell dependencies
