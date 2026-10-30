#include "../runtime.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct bd_closure {
    bd_code_ptr code;
    uint64_t arity;
    uint64_t argc;
    bd_value args[];
};

_Static_assert(offsetof(struct bd_closure, code) == 0, "closure code offset");
_Static_assert(offsetof(struct bd_closure, arity) == 8, "closure arity offset");
_Static_assert(offsetof(struct bd_closure, argc) == 16, "closure argc offset");
_Static_assert(offsetof(struct bd_closure, args) == 24, "closure args offset");

/* The adapter marshals arbitrary arities to the integer part of the RV64 ABI. */
extern bd_value bd_invoke(bd_code_ptr code, uint64_t argc, const bd_value *argv);

static _Noreturn void runtime_error(const char *message) {
    fputs(message, stderr);
    fputc('\n', stderr);
    exit(1);
}

static size_t checked_bytes(uint64_t count, size_t header) {
    /* The adapter also rounds outgoing stack arguments up to 16 bytes. */
    if (count > (SIZE_MAX - header - 15) / sizeof(bd_value)) {
        runtime_error("runtime: argument count is too large");
    }
    return header + (size_t)count * sizeof(bd_value);
}

static void *allocate(size_t bytes) {
    void *memory = malloc(bytes);
    if (memory == NULL) runtime_error("runtime: out of memory");
    return memory;
}

static struct bd_closure *new_closure(bd_code_ptr code, uint64_t arity,
                                      uint64_t argc) {
    struct bd_closure *result = allocate(checked_bytes(argc, sizeof(*result)));
    result->code = code;
    result->arity = arity;
    result->argc = argc;
    return result;
}

bd_value bd_make_closure(bd_code_ptr code, uint64_t arity) {
    if (code == NULL || arity == 0) runtime_error("runtime: invalid function");
    /* Reject an arity that cannot be marshalled by the adapter. */
    (void)checked_bytes(arity, 0);
    return (bd_value)(uintptr_t)new_closure(code, arity, 0);
}

bd_value bd_apply(bd_value value, uint64_t argc, const bd_value *argv) {
    if (argc == 0) return value;
    if (argv == NULL) runtime_error("runtime: missing arguments");

    for (;;) {
        struct bd_closure *closure = (struct bd_closure *)(uintptr_t)value;
        uint64_t needed = closure->arity - closure->argc;

        if (argc < needed) {
            uint64_t saved = closure->argc + argc;
            struct bd_closure *partial = new_closure(closure->code, closure->arity, saved);
            if (closure->argc != 0) {
                memcpy(partial->args, closure->args,
                       (size_t)closure->argc * sizeof(bd_value));
            }
            memcpy(partial->args + closure->argc, argv,
                   (size_t)argc * sizeof(bd_value));
            return (bd_value)(uintptr_t)partial;
        }

        bd_value *arguments = allocate(checked_bytes(closure->arity, 0));
        if (closure->argc != 0) {
            memcpy(arguments, closure->args,
                   (size_t)closure->argc * sizeof(bd_value));
        }
        memcpy(arguments + closure->argc, argv,
               (size_t)needed * sizeof(bd_value));
        bd_value result = bd_invoke(closure->code, closure->arity, arguments);
        free(arguments);

        argc -= needed;
        if (argc == 0) return result;
        argv += needed;
        value = result;
    }
}
