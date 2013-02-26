# The Genuine Sieves of Eratosthenes
Copyright © 2013 Bart Massey

These files comprise various Haskell implementations of "The
Genuine Sieve of Eratosthenes", based on ideas and some code
from the paper of this title by Melissa E. O'Neill,
J. Functional Programming 19:1, January 2009
<http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.

In particular, the files `oneill-sieve.hs` and
`bird-sieve.hs` are taken directly from the above-referenced
paper. The file `PQ.hs` contains a keyed priority queue
using `Data.Map` that I wrote to make the O'Neill sieve
work: it's not very fast. Please look at the comments in
these files and at the paper for more information.

The file `massey-sieve.hs` is my own implementation. It is a
very simple implementation of O'Neill's idea, using
`Data.Set` for the priority queue.

The file `c2-sieve.hs` is taken from the c2 wiki
<http://c2.com/cgi/wiki?SieveOfEratosthenesInManyProgrammingLanguages>.
This is a scarily fast implementation: about an order of
magnitude faster than the others here.

The file `c-sieve.c` contains a fairly straightforward C
implementation of the Sieve of Eratosthenes, without the
2-wheel optimization or the limit optimization. Sadly, it is
blindingly faster than any of the Haskell implementations.

The file `wheel.hs` contains an implementation of
`massey-sieve` together with code that constructs a "wheel"
to accelerate it. It is there mostly for illustrative
purposes; this code is not built by default.

To build these, just use the supplied Makefile by typing
"make". You will need a recent GHC release (I'm using 7.4.1
currently), as well as my `parseargs` package, available
from [Hackage](http://hackage.haskell.org/package/parseargs).

To run, just run any of the built programs. The output is
the last prime and the number of primes up to some
limit. The default limt of 2000000 seems about right, but
you can specify another. You can also specify an
optimization to `massey-sieve` only, that causes it to go
faster in this application. There is also an option to just
print the primes as a list (except for `c-sieve`) for
debugging purposes: the default limit is 100 here.

This work is licensed under the "MIT License".  Please see
the file COPYING in the source distribution of this software
for license terms.
