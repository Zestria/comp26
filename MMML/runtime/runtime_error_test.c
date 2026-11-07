#include "runtime.h"

#include <string.h>

static mmml_value identity(const mmml_value *args) { return args[0]; }

/* A global, deliberately not a GC root: lets the test hold a raw pointer to a
   closure that has become unreachable and then collect it. */
static volatile mmml_value stale_probe;

static void drop_closure(void) { stale_probe = mmml_make_closure(identity, 1); }

/* Overwrites dead frames and scratch slots that may still hold the closure
   address, so the conservative scan cannot see it. */
static void scrub_stack(void) {
  volatile uintptr_t junk[512];
  for (size_t i = 0; i < 512; i++) junk[i] = UINT64_C(0xdeadbeefdeadbeef);
  (void)junk;
}

int main(int argc, char **argv) {
  if (argc != 2) mmml_fail("expected one test name");
  if (strcmp(argv[1], "collect_uninit") == 0) {
    mmml_collect();
    return 0; /* unreachable: mmml_collect fails without mmml_init */
  }
  mmml_set_stack_base(&argc);
  mmml_init();
  if (strcmp(argv[1], "apply") == 0)
    (void)mmml_apply(mmml_int(1), MMML_UNIT);
  else if (strcmp(argv[1], "arity") == 0)
    (void)mmml_make_closure(identity, 0);
  else if (strcmp(argv[1], "integer") == 0)
    (void)mmml_unbox_int(MMML_TRUE);
  else if (strcmp(argv[1], "boolean") == 0)
    (void)mmml_unbox_bool(mmml_int(1));
  else if (strcmp(argv[1], "range") == 0)
    (void)mmml_int(INT64_C(1) << 62);
  else if (strcmp(argv[1], "stale") == 0) {
    drop_closure();
    scrub_stack();
    mmml_collect(); /* the closure is unreachable: only the global points at it */
    (void)mmml_apply(stale_probe, MMML_UNIT);
  } else
    mmml_fail("unknown test name");
  return 0;
}
