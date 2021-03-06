# Copyright © 2013 Bart Massey
# [This work is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HS_SIEVES = massey-sieve oneill-sieve oneill-alt-sieve \
	    bird-sieve c2-sieve imperative-sieve
SIEVES = $(HS_SIEVES) c-sieve

.SUFFIXES: .hs .hi

.hs.o:
	ghc -Wall -O2 $(GHCFLAGS) --make -c $*.hs

all: $(SIEVES) testPrime wheel

massey-sieve: massey-sieve.o
	ghc -Wall -O2 --make massey-sieve

oneill-sieve: oneill-sieve.o PQ.o
	ghc -Wall -O2 --make oneill-sieve

oneill-alt-sieve: oneill-alt-sieve.o MPQ.o DefaultMain.o
	ghc -Wall -O2 --make oneill-alt-sieve

oneill-alt-sieve.o: oneill-alt-sieve.hs MPQ.hi DefaultMain.hi
	ghc -Wall -O2 -DUSE_MPQ -c oneill-alt-sieve.hs

oneill-alt-sieve.hs: oneill-sieve.hs
	[ -f oneill-alt-sieve.hs ] || \
	ln -s oneill-sieve.hs oneill-alt-sieve.hs

bird-sieve: bird-sieve.o
	ghc -Wall -O2 --make bird-sieve

c2-sieve: c2-sieve.o
	ghc -Wall -O2 --make c2-sieve

imperative-sieve: imperative-sieve.o
	ghc -Wall -O2 --make imperative-sieve

c-sieve: c-sieve.c
	gcc -Wall -O2 -std=c99 -o c-sieve c-sieve.c

wheel: wheel.o
	ghc -Wall -O2 --make wheel

testPrime: testPrime.o
	ghc -Wall -O2 --make testPrime

clean:
	-rm -f *.hi *.o $(SIEVES) oneill-alt-sieve.hs wheel testPrime

$(HS_SIEVES): DefaultMain.o

DefaultMain.hi: DefaultMain.o

PQ.hi: PQ.o

MPQ.hi: MPQ.o

# DO NOT DELETE: Beginning of Haskell dependencies
wheel.o : wheel.hs
testPrime.o : testPrime.hs
PQ.o : PQ.hs
MPQ.o : MPQ.hs
DefaultMain.o : DefaultMain.hs
bird-sieve.o : bird-sieve.hs
bird-sieve.o : DefaultMain.hi
c2-sieve.o : c2-sieve.hs
c2-sieve.o : DefaultMain.hi
imperative-sieve.o : imperative-sieve.hs
imperative-sieve.o : DefaultMain.hi
massey-sieve.o : massey-sieve.hs
massey-sieve.o : DefaultMain.hi
oneill-sieve.o : oneill-sieve.hs
oneill-sieve.o : PQ.hi
oneill-sieve.o : DefaultMain.hi
# DO NOT DELETE: End of Haskell dependencies
