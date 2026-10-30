/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

/* Runs the generated test entries one by one and prints their results. */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int64_t (*shmel_tests[])(void);
extern int64_t shmel_tests_count;

/* The same as in backend/print_runtime.c: the generated code calls it on
   division by zero. */
void shmel_div_by_zero(void) {
  fflush(stdout);
  fputs("Fatal error: exception Division_by_zero\n", stderr);
  exit(2);
}

int main(void) {
  for (int64_t i = 0; i < shmel_tests_count; i++)
    printf("%ld\n", (long)shmel_tests[i]());
  return 0;
}
