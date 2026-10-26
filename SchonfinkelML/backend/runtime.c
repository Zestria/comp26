/*
  SPDX-License-Identifier: LGPL-3.0-or-later
 */
#include "runtime.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void schoml_print_int(void *arg) {
  int integer = (int)(intptr_t)arg;
  printf("%d\n", integer);
  return;
}

void schoml_exit(void *arg) {
  unsigned char code = (unsigned char)(uintptr_t)arg;
  exit(code);
}
