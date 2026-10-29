/* SPDX-License-Identifier: LGPL-3.0-or-later */
#include "closure.h"
#include "gc.h"

#include <stddef.h>

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
