#include "runtime.h"

#include <stdio.h>
#include <stdlib.h>

_Noreturn void bd_division_by_zero(void) {
    fputs("division by zero\n", stderr);
    exit(1);
}
