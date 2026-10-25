/* Copyright 2023-2024, Kakadu and contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <stdint.h>
#include <stdio.h>

void print_int(int64_t n) {
    printf("%ld", n >> 1);
    fflush(stdout);
}
