/* Copyright © 2013 Bart Massey
 *
 * [This work is licensed under the "MIT License"]
 * Please see the file COPYING in the source
 * distribution of this software for license terms.
 */

/* Sieve of Eratosthenes, with some simple optimizations, in
   C. Some other simple optimizations are avoided to make
   performance comparable with the Haskell implementations
   here (use of limit to bound strikeoffs) or because they
   are a pain (odd candidates only). */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

static void usage(void) {
    fprintf(stderr, "c-sieve: usage: c-sieve [<limit>]\n");
    exit(1);
}

static uint64_t *bits;

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
    uint64_t count = 0;
    for (uint64_t i = 2; i <= limit; i++) {
        if (!bit(i)) {
            count++;
            for (uint64_t j = i * i; j <= limit; j += i)
                set_bit(j);
        }
    }
    printf("%lu\n", count);
    return 0;
}
