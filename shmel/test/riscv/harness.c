/* Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

/* Runs the generated test entries one by one and prints their results. */

#include <stdint.h>
#include <stdio.h>

extern int64_t (*shmel_tests[])(void);
extern int64_t shmel_tests_count;

int main(void) {
  for (int64_t i = 0; i < shmel_tests_count; i++)
    printf("%ld\n", (long)shmel_tests[i]());
  return 0;
}
