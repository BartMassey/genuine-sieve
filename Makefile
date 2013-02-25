# Copyright © 2013 Bart Massey
# [This work is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HS_SIEVES = sieve oneill-sieve bird-sieve c2-sieve 
SIEVES = $(HS_SIEVES) c-sieve

all: $(SIEVES)

sieve: sieve.hs
	ghc -Wall -O2 --make -o sieve sieve.hs

oneill-sieve: oneill-sieve.hs PQ.hs
	ghc -Wall -O2 --make -o oneill-sieve oneill-sieve.hs

bird-sieve: bird-sieve.hs
	ghc -Wall -O2 --make -o bird-sieve bird-sieve.hs

c2-sieve: c2-sieve.hs
	ghc -Wall -O2 --make -o c2-sieve c2-sieve.hs

c-sieve: c-sieve.c
	gcc -Wall -O2 -std=c99 -o c-sieve c-sieve.c

$(HS_SIEVES): DefaultMain.hs

clean:
	-rm -f *.hi *.o $(SIEVES)
