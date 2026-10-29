/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "closure.h"
#include "gc.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Closures travel as tagged pointers (bit 0 = 1), but the GC only accepts
 * allocation base addresses: untag before visiting. */
static void *gc_base(void *v) {
    return schoml_is_closure(v) ? (void *)schoml_untag(v) : v;
}

/* Managed captures are either tagged closures or non-tagged values (tuples,
 * integers used in CPS-style captures). For the CPS target, we accept the
 * heuristic: only tagged values are traced. TODO: proper type analysis or
 * boxing of non-closure managed captures. */
static void trace_closure(void *object, schoml_gc_visit_fn visit) {
    schoml_closure *c = object;
    for (size_t i = 0; i < c->applied; i++)
        if (c->env[i].is_ref && schoml_is_closure(c->env[i].value))
            visit(gc_base(c->env[i].value));
}

void *schoml_closure_make(void *code, size_t arity,
                          size_t n_caps, schoml_closure_slot *caps) {
    schoml_closure *c = schoml_gc_alloc(
        sizeof(*c) + n_caps * sizeof(schoml_closure_slot),
        trace_closure);
    c->arity = arity;
    c->applied = n_caps;
    c->code = code;
    for (size_t i = 0; i < n_caps; i++) c->env[i] = caps[i];
    return (void *)((uintptr_t)c | 1);
}

static _Noreturn void fail(const char *message) {
    fprintf(stderr, "SchonfinkelML closure: %s\n", message);
    abort();
}

/* The code pointer of a closure and a raw function value are called through
 * the matching C function type. Object-to-function pointer conversion is not
 * portable as a cast, so copy the bits (POSIX guarantees equal sizes). */
_Static_assert(sizeof(void (*)(void)) == sizeof(void *),
               "function and object pointers must have the same size");

typedef void *(*schoml_fn1)(void *);
typedef void *(*schoml_fn2)(void *, void *);
typedef void *(*schoml_fn3)(void *, void *, void *);
typedef void *(*schoml_fn4)(void *, void *, void *, void *);
typedef void *(*schoml_fn5)(void *, void *, void *, void *, void *);
typedef void *(*schoml_fn6)(void *, void *, void *, void *, void *, void *);
typedef void *(*schoml_fn7)(void *, void *, void *, void *, void *, void *,
                            void *);
typedef void *(*schoml_fn8)(void *, void *, void *, void *, void *, void *,
                            void *, void *);

#define DEFINE_BRIDGE(name, type)          \
    static type name(void *p) {            \
        type f;                            \
        memcpy(&f, &p, sizeof f);          \
        return f;                          \
    }
DEFINE_BRIDGE(as_fn1, schoml_fn1)
DEFINE_BRIDGE(as_fn2, schoml_fn2)
DEFINE_BRIDGE(as_fn3, schoml_fn3)
DEFINE_BRIDGE(as_fn4, schoml_fn4)
DEFINE_BRIDGE(as_fn5, schoml_fn5)
DEFINE_BRIDGE(as_fn6, schoml_fn6)
DEFINE_BRIDGE(as_fn7, schoml_fn7)
DEFINE_BRIDGE(as_fn8, schoml_fn8)

/* Full application: c->arity == c->applied + n and 1 <= c->arity <= 8.
 * Arguments are the captured values followed by the new ones. */
static void *call_with(schoml_closure *c, size_t n, void **argv) {
    void *args[SCHOML_CLOSURE_MAX_ARITY];
    for (size_t i = 0; i < c->applied; i++) args[i] = c->env[i].value;
    for (size_t i = 0; i < n; i++) args[c->applied + i] = argv[i];
    switch (c->arity) {
    case 1: return as_fn1(c->code)(args[0]);
    case 2: return as_fn2(c->code)(args[0], args[1]);
    case 3: return as_fn3(c->code)(args[0], args[1], args[2]);
    case 4: return as_fn4(c->code)(args[0], args[1], args[2], args[3]);
    case 5:
        return as_fn5(c->code)(args[0], args[1], args[2], args[3], args[4]);
    case 6:
        return as_fn6(c->code)(args[0], args[1], args[2], args[3], args[4],
                               args[5]);
    case 7:
        return as_fn7(c->code)(args[0], args[1], args[2], args[3], args[4],
                               args[5], args[6]);
    case 8:
        return as_fn8(c->code)(args[0], args[1], args[2], args[3], args[4],
                               args[5], args[6], args[7]);
    default: fail("unsupported arity");
    }
}

static void *apply_n(void *f, size_t n, void **argv) {
    if (!schoml_is_closure(f)) {
        /* A raw code pointer: full application only. Partial application
         * through a raw pointer is undefined. */
        switch (n) {
        case 1: return as_fn1(f)(argv[0]);
        case 2: return as_fn2(f)(argv[0], argv[1]);
        default: fail("raw application with too many arguments");
        }
    }
    schoml_closure *c = schoml_untag(f);
    if (c->arity == 0 || c->arity > SCHOML_CLOSURE_MAX_ARITY)
        fail("unsupported arity");
    if (c->applied > c->arity) fail("applied exceeds arity");
    size_t need = c->arity - c->applied;
    if (need > n) {
        /* Partial application: capture the new arguments on top of the
         * existing environment. The parent closure and managed arguments
         * stay rooted across the allocation. */
        schoml_gc_root roots[1 + 2];
        size_t nroots = 0;
        schoml_gc_push_root(&roots[nroots++], c);
        for (size_t i = 0; i < n; i++)
            if (schoml_is_closure(argv[i]))
                schoml_gc_push_root(&roots[nroots++], gc_base(argv[i]));
        schoml_closure_slot caps[SCHOML_CLOSURE_MAX_ARITY];
        for (size_t i = 0; i < c->applied; i++) caps[i] = c->env[i];
        for (size_t i = 0; i < n; i++)
            caps[c->applied + i] = (schoml_closure_slot){
                .is_ref = schoml_is_closure(argv[i]),
                .value = argv[i],
            };
        void *result =
            schoml_closure_make(c->code, c->arity, c->applied + n, caps);
        while (nroots > 0) {
            schoml_gc_pop_root();
            nroots--;
        }
        return result;
    }
    if (need == n) return call_with(c, n, argv);
    /* Over-application: call with the missing arguments, then apply the
     * result to the rest. The result must be a function value. */
    void *result = call_with(c, need, argv);
    return apply_n(result, n - need, argv + need);
}

void *schoml_closure_apply1(void *f, void *a) {
    void *argv[1] = {a};
    return apply_n(f, 1, argv);
}

void *schoml_closure_apply2(void *f, void *a, void *b) {
    void *argv[2] = {a, b};
    return apply_n(f, 2, argv);
}
