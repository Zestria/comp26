#include "runtime_internal.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Conservative, non-moving mark-sweep collector.
   Roots: every 8-aligned word between this frame and the outermost recorded
   frame. Objects: the closures on the mmml_allocated list. Only
   args[0..filled-1] are traced; unfilled slots are uninitialized. */

/* ---- collection policy ---- */

#define MMML_DEFAULT_GC_THRESHOLD ((size_t)1 << 20)

static size_t pending_bytes;
static size_t gc_threshold = MMML_DEFAULT_GC_THRESHOLD;
static const void *stack_base;
static bool have_stack_base;

void mmml_gc_reset(void) { pending_bytes = 0; }

void mmml_maybe_collect(size_t upcoming_bytes) {
  if (pending_bytes + upcoming_bytes > gc_threshold) mmml_collect();
}

void mmml_account_allocation(size_t bytes) { pending_bytes += bytes; }

void mmml_set_stack_base(const void *outermost) {
  if (have_stack_base) mmml_fail("stack base already recorded");
  if (outermost == NULL) mmml_fail("stack base must not be null");
  stack_base = outermost;
  have_stack_base = true;
}

void mmml_set_gc_threshold(size_t bytes) {
  if (bytes == 0) mmml_fail("GC threshold must be positive");
  gc_threshold = bytes;
}

/* ---- address set of allocated closures ---- */

typedef struct address_set {
  size_t capacity; /* power of two */
  uintptr_t *slots; /* 0 = empty; closure addresses are non-zero */
} address_set;

static size_t hash_address(uintptr_t address) {
  uint64_t x = address;
  x ^= x >> 33;
  x *= UINT64_C(0xff51afd7ed558ccd);
  x ^= x >> 33;
  return (size_t)x;
}

static bool set_contains(const address_set *set, uintptr_t address) {
  size_t mask = set->capacity - 1;
  for (size_t i = hash_address(address) & mask;; i = (i + 1) & mask) {
    uintptr_t slot = set->slots[i];
    if (slot == 0) return false;
    if (slot == address) return true;
  }
}

static void set_insert(address_set *set, uintptr_t address) {
  size_t mask = set->capacity - 1;
  size_t i = hash_address(address) & mask;
  while (set->slots[i] != 0) i = (i + 1) & mask;
  set->slots[i] = address;
}

/* Load factor stays below 0.5, so insertion and probing always terminate. */
static address_set set_build(void) {
  size_t count = 0;
  for (mmml_closure *it = mmml_allocated; it != NULL; it = it->next) count++;
  size_t capacity = 16;
  while (capacity < 2 * count + 1) capacity <<= 1;
  address_set set = {capacity, calloc(capacity, sizeof(uintptr_t))};
  if (set.slots == NULL) mmml_fail("out of memory building GC address set");
  for (mmml_closure *it = mmml_allocated; it != NULL; it = it->next)
    set_insert(&set, (uintptr_t)it);
  return set;
}

/* ---- mark worklist ---- */

typedef struct mark_list {
  mmml_closure **items;
  size_t length;
  size_t capacity;
} mark_list;

static void mark_push(mark_list *list, mmml_closure *closure) {
  if (list->length == list->capacity) {
    size_t next = list->capacity == 0 ? 64 : 2 * list->capacity;
    mmml_closure **items = realloc(list->items, next * sizeof(*items));
    if (items == NULL) mmml_fail("out of memory growing GC mark stack");
    list->items = items;
    list->capacity = next;
  }
  list->items[list->length++] = closure;
}

static void mark_value(mark_list *list, const address_set *set,
                       mmml_value value) {
  if (!mmml_is_closure(value)) return;
  uintptr_t address = (uintptr_t)value;
  if (!set_contains(set, address)) return;
  mmml_closure *closure = (mmml_closure *)address;
  if (closure->marked) return;
  closure->marked = true;
  mark_push(list, closure);
}

/* ---- collection ---- */

void mmml_collect(void) {
  mmml_require_init();
  if (!have_stack_base) mmml_fail("stack base not recorded");

  address_set set = set_build();
  mark_list work = {NULL, 0, 0};

  /* Conservative roots. Generated code stores live values in frame slots,
     C keeps live values on the stack across calls. Registers are not roots. */
  uintptr_t anchor = 0;
  uintptr_t low = (uintptr_t)&anchor;
  uintptr_t high = (uintptr_t)stack_base;
  for (uintptr_t p = low; p < high; p += sizeof(uintptr_t)) {
    mmml_value word;
    memcpy(&word, (const void *)p, sizeof(word));
    mark_value(&work, &set, word);
  }

  /* Transitive closure over filled argument slots only. */
  while (work.length > 0) {
    mmml_closure *closure = work.items[--work.length];
    for (size_t i = 0; i < closure->filled; i++)
      mark_value(&work, &set, closure->args[i]);
  }

  /* Sweep: unlink and free unmarked closures, clear marks on survivors. */
  mmml_closure **link = &mmml_allocated;
  while (*link != NULL) {
    mmml_closure *closure = *link;
    if (!closure->marked) {
      *link = closure->next;
      free(closure);
    } else {
      closure->marked = false;
      link = &closure->next;
    }
  }
  pending_bytes = 0;

  free(work.items);
  free(set.slots);
}
