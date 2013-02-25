/* Copyright © 2013 Bart Massey
 *
 * [This work is licensed under the "MIT License"]
 * Please see the file COPYING in the source
 * distribution of this software for license terms.
 */

/* Sieve of Eratosthenes, with some simple optimizations, in
   C. The use of limit to bound strikeoffs is avoided to make
   performance comparable with the Haskell implementations,
   especially since this thing is already fast enough. */


#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

static void usage(void) {
    fprintf(stderr, "c-sieve: usage: c-sieve [<limit>]\n");
    exit(1);
}

static uint64_t *bits;
static uint64_t *primes;

static inline int bit(uint64_t i) {
    return (int)((bits[i >> 6LL] >> (i & 63LL)) & 1LL);
}

static inline void set_bit(uint64_t i) {
    uint64_t b = 1LL << (i & 63LL);
    bits[i >> 6LL] |= b;
}

int main(int argc, char **argv) {
    int64_t limit = 2000000;
    if (argc > 1) {
        limit = atoi(argv[1]);
        if (limit <= 0)
            usage();
    }
    if (argc > 2)
        usage();
    bits = calloc(limit / 64 + 2, 8);
    assert(bits);
    primes = calloc(limit + 1, 8);
    assert(primes);
    uint64_t count = 0;
    primes[count++] = 2;
    for (uint64_t i = 3; i <= limit; i += 2) {
        if (!bit(i)) {
            primes[count++] = i;
            for (uint64_t j = i * i; j <= limit; j += 2 * i)
                set_bit(j);
        }
    }
    printf("%lu %lu\n", primes[count - 1], count);
    return 0;
}
