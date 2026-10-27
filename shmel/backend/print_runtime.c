#include "closure_runtime.h"

#include <inttypes.h>
#include <stdio.h>

shmel_word shmel_print_int(shmel_word value) {
    printf("%" PRId64, (int64_t)value);
    return 0;
}
