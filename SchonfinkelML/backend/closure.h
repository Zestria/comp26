/* SPDX-License-Identifier: LGPL-3.0-or-later */
#ifndef SCHOML_CLOSURE_H
#define SCHOML_CLOSURE_H

#include <stdint.h>

typedef struct schoml_closure schoml_closure;

/* Each env slot explicitly marks whether the value is a managed pointer. */
typedef struct {
    int is_ref;
    void *value;
} schoml_closure_slot;

struct schoml_closure {
    int64_t arity;      /* total arguments expected */
    int64_t applied;    /* captured args already in env */
    void *code;         /* raw code pointer into .text, never traced */
    schoml_closure_slot env[];
};

/* Returns a tagged pointer (bit 0 = 1). Caller must root managed captures. */
void *schoml_closure_make(void *code, int64_t arity,
                          int64_t n_caps, schoml_closure_slot *caps);

static inline int schoml_is_closure(void *v) {
    return ((uintptr_t)v & 1) != 0;
}
static inline schoml_closure *schoml_untag(void *v) {
    return (schoml_closure *)((uintptr_t)v & ~(uintptr_t)1);
}

#endif
