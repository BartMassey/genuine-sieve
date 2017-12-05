#!/bin/sh
# Copyright © 2015 Bart Massey
# This code is made available under the "MIT
# License". Please see the file COPYING in this distribution
# for license details.

# Run all the implementations, reporting timing.

# I was using a non-standard version of the "time" command I
# rewrote to get extra precision.  I plan to upstream
# it. Now that I've ironed out timing loop lengths, it
# should actually be adequate to use standard time in the
# meantime.

#TIME="etime -f %3e"
TIME="time -f %e"

# Note: "2>&1 >/dev/null" sends the time output that was on
# stderr to stdout and throws away the result "0" that the
# program prints on stdout.
while read SIZE SIEVE ARGS
do
    echo -n "$SIEVE: $SIZE "
    $TIME ./$SIEVE $SIZE $ARGS 2>&1 >/dev/null
done <<EOF |
1000000   bird-sieve
100000000 c-sieve
1000000   c2-sieve
10000000  imperative-sieve -l
1000000   massey-sieve
1000000   oneill-alt-sieve
1000000   oneill-sieve
EOF
while read SIEVE ITERS TIME
do
    echo "$SIEVE" `python3 -c "print($TIME*1000000/$ITERS)"`
done |
sort -n -k 2
