/* SPDX-License-Identifier: LGPL-3.0-or-later */
#ifndef SCHOML_GC_H
#define SCHOML_GC_H

#include <stddef.h>

/* Visit only NULL or the base address of a live GC allocation. Integers,
 * interior pointers and bare function pointers must not be visited. */
typedef void (*schoml_gc_visit_fn)(void *object);
typedef void (*schoml_gc_trace_fn)(void *object, schoml_gc_visit_fn visit);

/* Roots form an explicit stack. Keep this record alive until it is popped.
 * The collector reads value at collection time; update it after reassignment.
 * A value held only in a C/assembly local or register is NOT a root. */
typedef struct schoml_gc_root {
  void *value;
  struct schoml_gc_root *previous;
} schoml_gc_root;

typedef struct {
  size_t objects;
  size_t bytes; /* Live payload bytes, excluding collector metadata. */
  size_t collections;
} schoml_gc_stats;

void schoml_gc_push_root(schoml_gc_root *root, void *value);
/* Pop the most recently registered root. An empty root stack aborts. */
void schoml_gc_pop_root(void);

/* Allocate zeroed, maximally aligned, nonmoving storage. A NULL trace denotes
 * an object without outgoing references. A trace may only read its object and
 * call visit on managed references: it must not allocate or change roots.
 * Allocation may collect BEFORE creating the new object, including on OOM.
 * Root every live managed argument before calling an allocating function.
 * Invalid sizes and unrecoverable allocation failures terminate the process. */
void *schoml_gc_alloc(size_t bytes, schoml_gc_trace_fn trace);
void schoml_gc_collect(void);
schoml_gc_stats schoml_gc_get_stats(void);

/* Set the next collection threshold in total heap bytes, including metadata.
 * Each collection then adapts the threshold to twice the live heap size,
 * with a 4 KiB minimum. Zero requests collection on the next allocation. */
void schoml_gc_set_threshold(size_t bytes);

/* Release the heap and reset statistics. All roots must have been popped. */
void schoml_gc_shutdown(void);

#endif
