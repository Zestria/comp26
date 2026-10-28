/* Test double for developer C's runtime contract. Not a production runtime.
   Kept separate so backend tests run while tasks 7/8 are still placeholders. */
#include "../../runtime/runtime.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct closure {
    bd_code_ptr code;
    uint64_t arity;
    uint64_t argc;
    bd_value args[];
};

extern bd_value test_invoke(bd_code_ptr, uint64_t, const bd_value *);

static void *allocate(size_t bytes) {
    void *result = malloc(bytes);
    if (!result) abort();
    return result;
}

bd_value bd_make_closure(bd_code_ptr code, uint64_t arity) {
    struct closure *result = allocate(sizeof(*result));
    result->code = code;
    result->arity = arity;
    result->argc = 0;
    return (bd_value)(uintptr_t)result;
}

bd_value bd_apply(bd_value value, uint64_t argc, const bd_value *argv) {
    struct closure *closure = (struct closure *)(uintptr_t)value;
    if (!argc) return value;
    uint64_t total = closure->argc + argc;
    if (total < closure->arity) {
        struct closure *result = allocate(sizeof(*result) + total * sizeof(bd_value));
        result->code = closure->code;
        result->arity = closure->arity;
        result->argc = total;
        memcpy(result->args, closure->args, closure->argc * sizeof(bd_value));
        memcpy(result->args + closure->argc, argv, argc * sizeof(bd_value));
        return (bd_value)(uintptr_t)result;
    }
    bd_value *args = allocate(closure->arity * sizeof(bd_value));
    memcpy(args, closure->args, closure->argc * sizeof(bd_value));
    uint64_t consumed = closure->arity - closure->argc;
    memcpy(args + closure->argc, argv, consumed * sizeof(bd_value));
    bd_value result = test_invoke(closure->code, closure->arity, args);
    free(args);
    if (argc > consumed) return bd_apply(result, argc - consumed, argv + consumed);
    return result;
}

bd_value bd_print_int(bd_value value) {
    printf("%" PRId64, (int64_t)value);
    return 0;
}

_Noreturn void bd_division_by_zero(void) {
    fputs("division by zero\n", stderr);
    exit(1);
}
