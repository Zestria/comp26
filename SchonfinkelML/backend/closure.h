/* SPDX-License-Identifier: LGPL-3.0-or-later */
#ifndef SCHOML_CLOSURE_H
#define SCHOML_CLOSURE_H

#include <stdint.h>
#include <stddef.h>

typedef struct schoml_closure schoml_closure;

/* Each env slot explicitly marks whether the value is a managed pointer.
 * Offsets (RV64): is_ref = 0, value = 8; a slot is 16 bytes. */
typedef struct {
    int is_ref;
    void *value;
} schoml_closure_slot;

/* Offsets (RV64): arity = 0, applied = 8, code = 16, env = 24. */
struct schoml_closure {
    size_t arity;       /* total arguments expected */
    size_t applied;     /* captured args already in env */
    void *code;         /* raw code pointer into .text, never traced */
    schoml_closure_slot env[];
};

/* Returns a tagged pointer (bit 0 = 1). Caller must root managed captures.
 * `caps` is an array of n_caps slots and is only read during the call. */
void *schoml_closure_make(void *code, size_t arity,
                          size_t n_caps, schoml_closure_slot *caps);

/* Maximum arity supported by the application entry points below: all
 * arguments have to fit into a0..a7. */
#define SCHOML_CLOSURE_MAX_ARITY 8

/* caml_callback-style application. Handles partial (creates a new closure,
 * rooting live values across allocation), full, and over-application.
 * Raw code pointers are called directly; partial through raw is undefined. */
void *schoml_closure_apply1(void *f, void *a);
void *schoml_closure_apply2(void *f, void *a, void *b);

static inline int schoml_is_closure(void *v) {
    return ((uintptr_t)v & 1) != 0;
}
static inline schoml_closure *schoml_untag(void *v) {
    return (schoml_closure *)((uintptr_t)v & ~(uintptr_t)1);
}

#endif
