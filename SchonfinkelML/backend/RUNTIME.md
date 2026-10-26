# Managed runtime

The runtime provides a single-threaded, nonmoving mark-and-sweep heap. It has
explicit roots and precise object tracers: neither the machine stack nor
registers are scanned. Integers and bare code pointers are not GC references.
Reachable objects keep their addresses; unreachable objects, including cycles,
are freed. Marking uses an iterative worklist, so deep graphs do not consume the
C call stack. Collection needs no additional heap allocations.

## Build and test

From the repository root:

```sh
dune b -p SchonfinkelML
dune test -p SchonfinkelML
```

The build produces
`_build/default/SchonfinkelML/backend/libschoml_runtime.a` for the **host**
architecture. The archive, headers, and C sources are included in the package.
Runtime tests are part of `dune test -p SchonfinkelML`; they cover roots,
automatic collection, sharing, cycles, deep graphs, tuples, nested closure
captures, allocation size overflow, and invalid runtime calls.

For a RISC-V executable, compile the C sources with a cross compiler. For example,
given a C driver with a `main` function:

```sh
riscv64-linux-gnu-gcc -std=c11 -O2 -march=rv64gc -mabi=lp64d \
  -I SchonfinkelML/backend driver.c \
  SchonfinkelML/backend/runtime.c SchonfinkelML/backend/gc.c \
  SchonfinkelML/backend/tuple.c -o program.exe
qemu-riscv64 -L /usr/riscv64-linux-gnu ./program.exe
```

The runtime uses libc (`calloc`, `free`, stdio, and process termination).
The current arithmetic-only emitter emits `_start` and exits by syscall; it
does not call this runtime yet. Emitting function calls, establishing a libc
entry point, and inserting roots belong to the code generation integration.
Tuple syntax and closure application are separate frontend/backend tasks.

## Allocation and roots

`schoml_gc_alloc(bytes, trace)` returns zero-initialized storage aligned to
`max_align_t`. `trace == NULL` denotes an object without outgoing GC references.
Size overflow and unrecoverable out-of-memory conditions terminate the process.
There is no per-object manual free.

Allocation can collect **before** allocating the new object. Before any call
that may allocate:

1. Register every live managed pointer using
   `schoml_gc_push_root(&root, pointer)`. The root record must remain alive and
   writable until it is popped. A pointer only in a local variable, register,
   or unregistered global does not keep an object alive.
2. Keep `root.value` up to date after reassigning the corresponding variable.
   Keep managed arguments rooted throughout calls that may allocate. A returned
   object must be rooted before the next allocating call.
3. Pop roots in reverse registration order with `schoml_gc_pop_root(&root)`.
   Remove references when their values become dead. To release an earlier root
   while later roots remain active, set its `value` to `NULL`, then pop its
   record when it reaches the top of the root stack.

For assembly integration, `schoml_gc_root` contains two pointer-sized fields:
`value`, then `previous`. On RV64 these are offsets 0 and 8, with a total size
of 16 bytes. Reserve each root record in the caller's live stack frame and
pass its address to the C root functions using the normal calling convention.
Root records must survive nested calls; pop them before dismantling a frame,
returning, or tail-calling. Global roots can use static records registered before
function-local roots. Only GC allocation base addresses or `NULL` are valid root
values; interior pointers and raw function addresses must not be registered.

The initial collection threshold is 4 KiB of allocated storage, including GC
headers. After collection it becomes twice the live heap size, with a 4 KiB
minimum. An allocation failure triggers collection and one retry.
`schoml_gc_set_threshold(0)` forces a collection before the next allocation,
which is useful when testing root placement. `schoml_gc_collect()` collects
explicitly. `schoml_gc_get_stats()` reports live object count, payload bytes,
and collection count. `schoml_gc_shutdown()` requires all roots to be popped,
releases the heap, and resets statistics.

Here is a complete `driver.c` for the command above:

```c
#include "runtime.h"
#include <assert.h>

int main(void) {
  schoml_gc_root root;
  schoml_tuple *outer = schoml_tuple_alloc(1);
  schoml_gc_push_root(&root, outer);

  schoml_gc_set_threshold(0);
  schoml_tuple *inner = schoml_tuple_alloc(1);
  schoml_tuple_set_word(inner, 0, 42);
  schoml_tuple_set_ref(outer, 0, inner);
  schoml_gc_collect();
  assert(schoml_gc_get_stats().objects == 2);
  assert(schoml_tuple_get_word(schoml_tuple_get_ref(outer, 0), 0) == 42);

  schoml_gc_pop_root(&root);
  schoml_gc_collect();
  assert(schoml_gc_get_stats().objects == 0);
  schoml_gc_shutdown();
  return 0;
}
```

## Closure integration

The collector accepts arbitrary object layouts. Allocate a closure using
`schoml_gc_alloc(sizeof_closure, trace_closure)` and supply a tracer that visits
every managed capture or environment pointer. For example, for a layout with
a code pointer and one managed environment:

```c
static void trace_closure(void *object, schoml_gc_visit_fn visit) {
  my_closure *closure = object;
  visit(closure->environment);
}
```

The tracer must visit **only** live GC allocation base addresses or `NULL`.
Do not visit code pointers, immediate values, or interior pointers. A tracer
must not allocate, collect, modify roots, or mutate object references. For a
partially applied function, visit captured managed arguments as well as any
managed parent closure. Those arguments must already be rooted while the new
closure is allocated. The code pointer and immediate captures need no tracing.

This keeps captures alive as long as the closure is reachable. Once the closure
and its captures are unreachable, the next collection frees them, including
mutually recursive closure/environment cycles. Functions represented by bare
code pointers need neither a heap object nor a GC root.

The production closure layout and application machinery are owned by the
closure-runtime task. `tests/tuple_test.c` uses a small test-only closure to
verify that its environment survives automatic collection, that its code remains
callable, and that the entire unreachable cycle is reclaimed.

## Tuples

`schoml_tuple_alloc(length)` allocates a managed tuple with zero-valued immediate
fields. Tuples are opaque; use the non-allocating accessors in `tuple.h`:

| Field | Store | Load |
| --- | --- | --- |
| Immediate machine word | `schoml_tuple_set_word` | `schoml_tuple_get_word` |
| Managed pointer or `NULL` | `schoml_tuple_set_ref` | `schoml_tuple_get_ref` |

Each field has explicit reference metadata. Immediate words retain all
`uintptr_t` bits, so even an integer equal to an allocation address does not
retain that allocation. Replacing a reference with an immediate drops that
edge from the object graph. A reference field may point to any managed object,
including another tuple or a closure. Out-of-range accesses and getters with
the wrong field kind terminate the process.
