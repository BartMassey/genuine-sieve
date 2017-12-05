# The Genuine Sieves of Eratosthenes
Copyright © 2013 Bart Massey

These files comprise various Haskell implementations of "The
Genuine Sieve of Eratosthenes", based on ideas and some code
from the paper of this title by Melissa E. O'Neill,
J. Functional Programming 19:1, January 2009
<http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.

## Manifest

The files `oneill-sieve.hs` and
`bird-sieve.hs` are taken directly from the above-referenced
paper. The file `PQ.hs` contains a keyed priority queue
using `Data.Map` that I wrote to make the O'Neill sieve
work: it's not very fast. The file `MPQ.hs` contains an
alternate implementation of O'Neill's priority queue using
`Data.Map`, in which the map values are merged streams
instead of multiple unmerged streams--performance is not
significantly different. (This code builds as
`oneill-alt-sieve`.)  Please look at the comments in these
files and at the paper for more information.

The file `massey-sieve.hs` is my own implementation. It is a
very simple implementation of O'Neill's idea, using
`Data.Set` for the priority queue.

The file `c2-sieve.hs` is taken from the c2 wiki
<http://c2.com/cgi/wiki?SieveOfEratosthenesInManyProgrammingLanguages>.
This is a scarily fast implementation: about a factor of 5
faster than the others here.

The file `c-sieve.c` contains a fairly straightforward C
implementation of the Sieve of Eratosthenes, without the
2-wheel optimization or the limit optimization. Sadly, it is
blindingly faster than any of the Haskell implementations.

The file `imperative-sieve.hs` contains a fairly
straightforward Haskell implementation of the Sieve of
Eratosthenes using an `STUArray` of `Bool`.

The file `wheel.hs` contains an implementation of
`massey-sieve` together with code that constructs a "wheel"
to accelerate it. It is there mostly for illustrative and
timing purposes. You can run it with "./wheel <wheel-size>
limit".

The file `testPrime.hs` contains a copy of the wheel sieve
(I don't know why I chose this one rather than the c2 sieve
or the imperative sieve) together with a driver that reports
on whether the given `Word64` argument is prime. It is
intended for testing the primality of phone numbers, and in
particular will report that 8675309 is prime.

## Building and Running

To build these, just use the supplied Makefile by typing
"make". You will need a recent GHC release (I'm using 8.2.2
currently), as well as my `parseargs` package, available
from [Hackage](http://hackage.haskell.org/package/parseargs).

To run, just run any of the built sieve programs. The output
is the last prime and the number of primes up to some
limit. The default limit of 2000000 seems about right, but
you can specify another. You can also specify a limit option
as an optimization to `massey-sieve` only, that causes it to
go faster in this application. For `imperative-sieve`, you
must specify the limit option, as it cannot generate an
infinite stream. There is also an option to just print the
primes as a list (except for `c-sieve`) for debugging
purposes: the default limit is 100 here.

## Benchmarking

To bench these, build them and then run `sh bench.sh` on a
Linux box.  You'll need Python 3.

Here's some current times. Times are "seconds per million primes",
which is a bad measure but easy to implement. I've run
comparison tests with equal instance sizes and these numbers
seem relatively OK.

    c-sieve: 0.0025
    imperative-sieve: 0.01
    c2-sieve: 0.11
    bird-sieve: 0.38
    oneill-sieve: 0.56
    oneill-alt-sieve: 0.57
    massey-sieve: 0.61

The Haskell imperative sieve is currently about four times
slower than the C implementation. The c2 sieve, the fastest
of the Haskell "Genuine Sieves", is about 20 times slower
than the imperative sieve. All of the oneill-style sieves,
including mine, are about the same speed: ridiculously
slower than a naïve C implementation.

## License

This work is licensed under the "MIT License".  Please see
the file COPYING in the source distribution of this software
for license terms.
