/*
  SPDX-License-Identifier: LGPL-3.0-or-later
 */
#include <stdio.h>
#include <stdlib.h>

void schoml_print_int(void *arg) {
  int integer = (int)arg; // NOLINT(clang-diagnostic-void-pointer-to-int-cast)
  printf("%d\n", integer);
  return;
}

void schoml_exit(void *arg) {
  unsigned char code =
      (unsigned char)arg; // NOLINT(clang-diagnostic-void-pointer-to-int-cast)
  exit(code);
}
