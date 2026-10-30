#include "../runtime.h"

#include <inttypes.h>
#include <stdio.h>

bd_value bd_print_int(bd_value value) {
    printf("%" PRId64, (int64_t)value);
    return 0;
}
