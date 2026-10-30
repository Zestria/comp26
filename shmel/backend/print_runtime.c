#include "closure_runtime.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

shmel_word shmel_print_int(shmel_word value) {
    printf("%" PRId64, (int64_t)value);
    return 0;
}

/* Called by the generated code on division by zero; reports it like an
   uncaught OCaml exception. */
void shmel_div_by_zero(void) {
    fflush(stdout);
    fputs("Fatal error: exception Division_by_zero\n", stderr);
    exit(2);
}
