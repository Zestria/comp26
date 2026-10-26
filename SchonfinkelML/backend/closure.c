/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "closure.h"
#include "gc.h"

#include <stddef.h>

/* code lives in .text; tracing it would make the GC treat machine code
 * as a heap block. Only managed slots are visited. */
static void trace_closure(void *object, schoml_gc_visit_fn visit) {
    schoml_closure *c = object;
    for (int64_t i = 0; i < c->applied; i++)
        if (c->env[i].is_ref) visit(c->env[i].value);
}

void *schoml_closure_make(void *code, int64_t arity,
                          int64_t n_caps, schoml_closure_slot *caps) {
    schoml_closure *c = schoml_gc_alloc(
        sizeof(*c) + (size_t)n_caps * sizeof(schoml_closure_slot),
        trace_closure);
    c->arity = arity;
    c->applied = n_caps;
    c->code = code;
    for (int64_t i = 0; i < n_caps; i++) c->env[i] = caps[i];
    return (void *)((uintptr_t)c | 1);   /* tag */
}
