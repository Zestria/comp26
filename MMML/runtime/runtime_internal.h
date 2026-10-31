#ifndef MMML_RUNTIME_INTERNAL_H
#define MMML_RUNTIME_INTERNAL_H

#include "runtime.h"

/* Shared between runtime.c (allocation, application) and gc.c (collection).
   Not part of the public contract in runtime.h. */

typedef struct mmml_closure {
  struct mmml_closure *next;
  mmml_code code;
  size_t arity;
  size_t filled;
  bool marked; /* collector mark; only args[0..filled-1] hold live values */
  mmml_value args[];
} mmml_closure;

/* Intrusive list of every live closure; owned by runtime.c, swept by gc.c. */
extern mmml_closure *mmml_allocated;

/* runtime.c internal entry points used by gc.c. */
void mmml_require_init(void);

/* gc.c internal entry points used by runtime.c. */
void mmml_gc_reset(void);
void mmml_maybe_collect(size_t upcoming_bytes);
void mmml_account_allocation(size_t bytes);

#endif
