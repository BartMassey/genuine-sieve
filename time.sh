#!/bin/sh
# Copyright © 2015 Bart Massey
# This code is made available under the "MIT
# License". Please see the file COPYING in this distribution
# for license details.

# Measure elapsed (wall clock, real) time on a POSIX system.

/usr/bin/time -p "$@" 2>&1 |
awk '$1=="real"{print $2}' >&2
