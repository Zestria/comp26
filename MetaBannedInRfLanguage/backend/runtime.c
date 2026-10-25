/* Copyright 2023-2024, Kakadu and contributors */
/* Copyright 2026, Tenyaeva Ekaterina */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void division_by_zero(void) {
    fputs("division by zero\n", stderr);
    exit(EXIT_FAILURE);
}

void print_int(int64_t n) {
    printf("%ld", n >> 1);
    fflush(stdout);
}

/* ========== Closure ========== */

/* Typed function pointer: checked by the compiler, unlike void*. */
typedef int64_t (*closure_fn)(int64_t *);

typedef struct {
    closure_fn code;  // entry point of the generated function
    int64_t arity;    // arguments still expected
    int64_t received; // values already captured in env
    int64_t env[];    // captured environment
} closure;

int64_t closure_get_arity(int64_t cf) {
    return ((closure *)(uintptr_t)cf)->arity;
}

int64_t *closure_get_env(int64_t cf) {
    return ((closure *)(uintptr_t)cf)->env;
}

/*
 * Allocate a closure capturing env_len values from env.
 * code is the entry point, arity is the number of expected arguments.
 */
int64_t closure_alloc(int64_t code, int64_t arity, int64_t env_len, int64_t *env) {
    closure *cl = (closure *)malloc(sizeof(closure) + (size_t)env_len * sizeof(int64_t));
    int i;

    if (!cl) {
        fprintf(stderr, "closure: out of memory\n");
        exit(1);
    }

    cl->code = (closure_fn)(uintptr_t)code;
    cl->arity = arity;
    cl->received = env_len;
    for (i = 0; i < env_len; i++) {
        cl->env[i] = env[i];
    }
    return (int64_t)(uintptr_t)cl;
}

/*
 * Call a closure with exactly arity arguments.
 * Packs env and args into one array, since the generated code expects
 * a single contiguous argument buffer.
 */
int64_t closure_call(int64_t cf, int64_t nargs, int64_t *args) {
    closure *cl = (closure *)(uintptr_t)cf;
    closure_fn code = cl->code;
    int64_t received = cl->received;
    size_t total = (size_t)received + (size_t)nargs;
    int64_t *packed;
    int64_t res;

    packed = (int64_t *)malloc(total * sizeof(int64_t));
    if (!packed) {
        fprintf(stderr, "closure: out of memory\n");
        exit(1);
    }

    memcpy(packed, cl->env, (size_t)received * sizeof(int64_t));
    memcpy(packed + received, args, (size_t)nargs * sizeof(int64_t));

    res = code(packed);
    free(packed);
    return res;
}

/*
 * Apply a closure to nargs arguments:
 *   - under-application: returns a new closure with more captured args;
 *   - exact application: calls the closure's code;
 *   - over-application: calls with the first arity args, applies the rest to the result.
 */
int64_t apply(int64_t cf, int64_t nargs, int64_t *args) {
    closure *cl = (closure *)(uintptr_t)cf;
    int64_t arity = cl->arity;

    if (nargs < arity) {
        // Under-application: copy the existing closure and append arguments.
        int64_t new_received = cl->received + nargs;
        size_t new_size = sizeof(closure) + (size_t)new_received * sizeof(int64_t);
        closure *ncl = (closure *)malloc(new_size);

        if (!ncl) {
            fprintf(stderr, "closure: out of memory\n");
            exit(1);
        }

        ncl->code = cl->code;
        ncl->arity = cl->arity - nargs;
        ncl->received = new_received;
        memcpy(ncl->env, cl->env, (size_t)cl->received * sizeof(int64_t));
        memcpy(ncl->env + cl->received, args, (size_t)nargs * sizeof(int64_t));
        return (int64_t)(uintptr_t)ncl;
    }

    if (nargs == arity) {
        return closure_call(cf, nargs, args);
    }

    // Over-application.
    {
        int64_t partial = closure_call(cf, arity, args);
        return apply(partial, nargs - arity, args + arity);
    }
}
