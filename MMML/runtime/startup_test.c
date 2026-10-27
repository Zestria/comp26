#include "runtime.h"

#include <inttypes.h>
#include <stdio.h>

/* A stand-in for the symbol that the generated program must define. */
mmml_value mmml_program_main(void) {
  printf("%" PRId64 "\n", mmml_unbox_int(mmml_int(7)));
  return MMML_UNIT;
}
