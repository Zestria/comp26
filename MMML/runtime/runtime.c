#include "runtime.h"
#include "runtime_internal.h"

#include <alloca.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

_Static_assert(sizeof(void *) == 8 && sizeof(mmml_value) == 8,
               "MMML runtime requires a 64-bit target");

mmml_closure *mmml_allocated;
static bool initialized;

_Noreturn void mmml_fail(const char *message) {
  fprintf(stderr, "MMML runtime: %s\n", message == NULL ? "unknown error" : message);
  exit(EXIT_FAILURE);
}

void mmml_require_init(void) {
  if (!initialized) mmml_fail("mmml_init must be called first");
}

void mmml_init(void) {
  if (initialized) mmml_fail("already initialized");
  mmml_allocated = NULL;
  mmml_gc_reset();
  initialized = true;
}

void mmml_shutdown(void) {
  mmml_require_init();
  while (mmml_allocated != NULL) {
    mmml_closure *next = mmml_allocated->next;
    free(mmml_allocated);
    mmml_allocated = next;
  }
  initialized = false;
}

mmml_value mmml_int(int64_t number) {
  if (number < -(INT64_C(1) << 62) || number > (INT64_C(1) << 62) - 1)
    mmml_fail("integer outside 63-bit tagged range");
  return ((uint64_t)number << 1) | UINT64_C(1);
}

int64_t mmml_unbox_int(mmml_value value) {
  if ((value & UINT64_C(1)) == 0) mmml_fail("expected integer");
  uint64_t bits = value >> 1;
  if ((bits & (UINT64_C(1) << 62)) == 0) return (int64_t)bits;
  return -1 - (int64_t)((~bits) & ((UINT64_C(1) << 63) - 1));
}

mmml_value mmml_bool(bool value) { return value ? MMML_TRUE : MMML_FALSE; }

bool mmml_unbox_bool(mmml_value value) {
  if (value != MMML_FALSE && value != MMML_TRUE) mmml_fail("expected boolean");
  return value == MMML_TRUE;
}

bool mmml_is_closure(mmml_value value) {
  return value != 0 && (value & UINT64_C(7)) == 0;
}


static mmml_closure *closure_of(mmml_value value) {
  if (!mmml_is_closure(value)) mmml_fail("expected closure");
  mmml_closure *candidate = (mmml_closure *)(uintptr_t)value;
  for (mmml_closure *it = mmml_allocated; it != NULL; it = it->next)
    if (it == candidate) return it;
  mmml_fail("invalid closure");
}

mmml_value mmml_make_closure(mmml_code code, size_t arity) {
  mmml_require_init();
  if (code == NULL || arity == 0) mmml_fail("closure needs code and positive arity");
  if (arity > (SIZE_MAX - sizeof(mmml_closure)) / sizeof(mmml_value))
    mmml_fail("closure too large");
  size_t bytes = sizeof(mmml_closure) + arity * sizeof(mmml_value);
  mmml_maybe_collect(bytes);
  mmml_closure *object = malloc(bytes);
  if (object == NULL) mmml_fail("out of memory allocating closure");
  mmml_account_allocation(bytes);
  object->next = mmml_allocated;
  object->code = code;
  object->arity = arity;
  object->filled = 0;
  object->marked = false;
  mmml_allocated = object;
  return (mmml_value)(uintptr_t)object;
}

mmml_value mmml_apply(mmml_value function, mmml_value argument) {
  mmml_require_init();
  mmml_closure *old = closure_of(function);
  if (old->filled + 1 < old->arity) {
    mmml_value result = mmml_make_closure(old->code, old->arity);
    mmml_closure *next = closure_of(result);
    next->filled = old->filled + 1;
    memcpy(next->args, old->args, old->filled * sizeof(mmml_value));
    next->args[old->filled] = argument;
    return result;
  }

  /* The buffer lives on the C stack so the conservative collector sees the
     applied values while the callee runs (a callee may trigger a collection). */
  mmml_value *args = alloca(old->arity * sizeof(*args));
  memcpy(args, old->args, old->filled * sizeof(*args));
  args[old->filled] = argument;
  return old->code(args);
}
