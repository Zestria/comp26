#include "runtime.h"
#include "runtime_internal.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>

/* These bodies stand in for code emitted by the compiler. */
static mmml_value identity(const mmml_value *args) { return args[0]; }

static mmml_value apply_first(const mmml_value *args) {
  return mmml_apply(args[0], args[1]);
}

/* Allocates enough garbage to trigger collections, then uses its arguments. */
static mmml_value stress(const mmml_value *args) {
  for (int i = 0; i < 2000; i++) (void)mmml_make_closure(identity, 1);
  return mmml_apply(args[0], args[1]);
}

/* CPS factorial (same shape as runtime_test.c). */
static mmml_value factorial(const mmml_value *args);

static mmml_value factorial_continue(const mmml_value *args) {
  return mmml_apply(args[0],
                    mmml_int(mmml_unbox_int(args[1]) * mmml_unbox_int(args[2])));
}

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

/* CPS fibonacci: fib n k calls k (fib n). */
static mmml_value fibonacci(const mmml_value *args);

/* args: k, a = fib(n-1), b = fib(n-2) -> k (a + b) */
static mmml_value fibonacci_after_second(const mmml_value *args) {
  return mmml_apply(args[0],
                    mmml_int(mmml_unbox_int(args[1]) + mmml_unbox_int(args[2])));
}

/* args: k, n, a = fib(n-1) -> compute fib(n-2) with continuation (k, a) */
static mmml_value fibonacci_after_first(const mmml_value *args) {
  mmml_value second = mmml_make_closure(fibonacci_after_second, 3);
  second = mmml_apply(second, args[0]);
  second = mmml_apply(second, args[2]);
  mmml_value next = mmml_make_closure(fibonacci, 2);
  next = mmml_apply(next, mmml_int(mmml_unbox_int(args[1]) - 2));
  return mmml_apply(next, second);
}

static mmml_value fibonacci(const mmml_value *args) {
  int64_t n = mmml_unbox_int(args[0]);
  if (n < 2) return mmml_apply(args[1], mmml_int(n));
  mmml_value first = mmml_make_closure(fibonacci_after_first, 3);
  first = mmml_apply(first, args[1]);
  first = mmml_apply(first, mmml_int(n));
  mmml_value next = mmml_make_closure(fibonacci, 2);
  next = mmml_apply(next, mmml_int(n - 1));
  return mmml_apply(next, first);
}

/* White-box helpers over the internal allocation list. */
static volatile mmml_value probe; /* a global: deliberately not a GC root */

static bool in_list(mmml_value value) {
  for (mmml_closure *it = mmml_allocated; it != NULL; it = it->next)
    if ((mmml_value)(uintptr_t)it == value) return true;
  return false;
}

static size_t live_count(void) {
  size_t count = 0;
  for (mmml_closure *it = mmml_allocated; it != NULL; it = it->next) count++;
  return count;
}

static void drop_closure(void) { probe = mmml_make_closure(identity, 1); }

/* Overwrites the dead frames and scratch slots that may still hold the probe
   address, so the conservative scan cannot see it. */
static void scrub_stack(void) {
  volatile uintptr_t junk[512];
  for (size_t i = 0; i < 512; i++) junk[i] = UINT64_C(0xdeadbeefdeadbeef);
  (void)junk;
}

static void test_dropped_freed(void) {
  drop_closure();
  assert(in_list(probe));
  scrub_stack();
  mmml_collect();
  assert(!in_list(probe));
}

static void test_capture_survives(void) {
  mmml_value captured = mmml_make_closure(identity, 1);
  mmml_value holder = mmml_make_closure(apply_first, 2);
  holder = mmml_apply(holder, captured);
  mmml_collect();
  assert(in_list(holder));
  assert(in_list(captured));
  assert(mmml_unbox_int(mmml_apply(holder, mmml_int(40))) == 40);
}

static void test_unfilled_untouched(void) {
  mmml_value captured = mmml_make_closure(identity, 1);
  mmml_value holder = mmml_make_closure(apply_first, 3);
  holder = mmml_apply(holder, captured); /* filled = 1; args[1..2] garbage */
  mmml_collect();
  assert(in_list(captured));
  holder = mmml_apply(holder, mmml_int(40));
  assert(mmml_is_closure(holder));
  assert(mmml_unbox_int(mmml_apply(holder, MMML_UNIT)) == 40);
}

static void test_pointer_stability(void) {
  mmml_value id = mmml_make_closure(identity, 1);
  mmml_value bits = id;
  mmml_collect();
  assert(id == bits);
  assert(mmml_is_closure(id));
  assert(mmml_unbox_int(mmml_apply(id, mmml_int(42))) == 42);
}

static void test_threshold_trigger(void) {
  mmml_set_gc_threshold(4096);
  for (int i = 0; i < 500; i++) (void)mmml_make_closure(identity, 1);
  mmml_collect();
  assert(live_count() < 10);
  mmml_value survivor = mmml_make_closure(identity, 1);
  assert(in_list(survivor));
  assert(mmml_unbox_int(mmml_apply(survivor, mmml_int(7))) == 7);
}

static void test_apply_buffer(void) {
  mmml_set_gc_threshold(4096);
  mmml_value captured = mmml_make_closure(identity, 1);
  mmml_value f = mmml_make_closure(stress, 2);
  f = mmml_apply(f, captured);
  assert(mmml_unbox_int(mmml_apply(f, mmml_int(41))) == 41);
}

static void test_repeatable(void) {
  mmml_value id = mmml_make_closure(identity, 1);
  mmml_collect();
  mmml_collect();
  assert(in_list(id));
  assert(mmml_unbox_int(mmml_apply(id, mmml_int(1))) == 1);
}

static void test_cps_with_gc(void) {
  mmml_set_gc_threshold(4096);
  mmml_value id = mmml_make_closure(identity, 1);
  mmml_value fact = mmml_make_closure(factorial, 2);
  fact = mmml_apply(fact, mmml_int(12));
  assert(mmml_unbox_int(mmml_apply(fact, id)) == 479001600);
  mmml_value fib = mmml_make_closure(fibonacci, 2);
  fib = mmml_apply(fib, mmml_int(10));
  assert(mmml_unbox_int(mmml_apply(fib, id)) == 55);
}

int main(int argc, char **argv) {
  (void)argv;
  mmml_set_stack_base(&argc);
  mmml_init();
  test_dropped_freed();
  test_capture_survives();
  test_unfilled_untouched();
  test_pointer_stability();
  test_threshold_trigger();
  test_apply_buffer();
  test_repeatable();
  test_cps_with_gc();
  mmml_shutdown();
  puts("MMML runtime: GC tests passed");
  return 0;
}
