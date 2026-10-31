#include "runtime.h"

#include <assert.h>
#include <stdio.h>

/* These bodies stand in for code emitted by the compiler. */
static mmml_value add(const mmml_value *args) {
  return mmml_int(mmml_unbox_int(args[0]) + mmml_unbox_int(args[1]));
}

static mmml_value make_adder(const mmml_value *args) {
  return mmml_apply(mmml_make_closure(add, 2), args[0]);
}

static mmml_value identity(const mmml_value *args) { return args[0]; }

static mmml_value factorial(const mmml_value *args);

static mmml_value factorial_continue(const mmml_value *args) {
  return mmml_apply(args[0],
                    mmml_int(mmml_unbox_int(args[1]) * mmml_unbox_int(args[2])));
}

/* CPS factorial: each continuation carries k and n. */
static mmml_value factorial(const mmml_value *args) {
  int64_t n = mmml_unbox_int(args[0]);
  if (n <= 1) return mmml_apply(args[1], mmml_int(1));
  mmml_value continuation = mmml_make_closure(factorial_continue, 3);
  continuation = mmml_apply(continuation, args[1]);
  continuation = mmml_apply(continuation, mmml_int(n));
  mmml_value next = mmml_make_closure(factorial, 2);
  next = mmml_apply(next, mmml_int(n - 1));
  return mmml_apply(next, continuation);
}

static void test_values(void) {
  assert(mmml_unbox_int(mmml_int(-(INT64_C(1) << 62))) ==
         -(INT64_C(1) << 62));
  assert(mmml_unbox_int(mmml_int((INT64_C(1) << 62) - 1)) ==
         (INT64_C(1) << 62) - 1);
  assert(mmml_bool(false) == MMML_FALSE);
  assert(mmml_bool(true) == MMML_TRUE);
  assert(!mmml_unbox_bool(MMML_FALSE));
  assert(mmml_unbox_bool(MMML_TRUE));
  assert(!mmml_is_closure(mmml_int(1)));
  assert(!mmml_is_closure(MMML_UNIT));
}

static void test_closures(void) {
  mmml_value make = mmml_make_closure(make_adder, 1);
  mmml_value add10 = mmml_apply(make, mmml_int(10));
  mmml_value add20 = mmml_apply(make, mmml_int(20));
  assert(mmml_is_closure(add10));
  assert(mmml_unbox_int(mmml_apply(add10, mmml_int(5))) == 15);
  assert(mmml_unbox_int(mmml_apply(add20, mmml_int(5))) == 25);
  assert(mmml_unbox_int(mmml_apply(add10, mmml_int(8))) == 18);

  mmml_value plus = mmml_make_closure(add, 2);
  mmml_value add7 = mmml_apply(plus, mmml_int(7));
  assert(mmml_is_closure(add7));
  assert(mmml_unbox_int(mmml_apply(add7, mmml_int(8))) == 15);

  mmml_value fact = mmml_apply(mmml_make_closure(factorial, 2), mmml_int(5));
  mmml_value id = mmml_make_closure(identity, 1);
  assert(mmml_unbox_int(mmml_apply(fact, id)) == 120);
}

int main(int argc, char **argv) {
  (void)argv;
  mmml_set_stack_base(&argc);
  mmml_init();
  test_values();
  test_closures();
  mmml_shutdown();
  puts("MMML runtime: values, closures and CPS passed");
  return 0;
}
