/* Copyright 2023-2024, Kakadu and contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void division_by_zero(void) {
    fputs("division by zero\n", stderr);
    exit(EXIT_FAILURE);
}

void print_int(int64_t n) {
    printf("%ld", n >> 1);
    fflush(stdout);
}
