# Copyright © 2013 Bart Massey
# [This work is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.


SIEVES = sieve oneill-sieve bird-sieve

all: $(SIEVES)

sieve: sieve.hs
	ghc -Wall -O2 -o sieve sieve.hs

oneill-sieve: oneill-sieve.hs PQ.hs
	ghc -Wall -O2 --make -o oneill-sieve oneill-sieve.hs

bird-sieve: bird-sieve.hs
	ghc -Wall -O2 -o bird-sieve bird-sieve.hs

clean:
	-rm -f *.hi *.o $(SIEVES)
