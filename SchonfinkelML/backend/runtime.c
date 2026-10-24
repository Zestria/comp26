/*
  SPDX-License-Identifier: LGPL-3.0-or-later
 */
#include <stdio.h>

void print_int(void *arg) {
  int integer = (int)arg;
  printf("%d\n", integer);
  return;
}
