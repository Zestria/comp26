# Design

## Context

Motivation and scope: see proposal.md. The relevant runtime facts (from `origin/maltsev/codegen`, commit 44af9b8):

- The heap contains exactly one object kind: `mmml_closure` (`next`, `code`, `arity`, `filled`, `args[]`), allocated with `malloc` and linked into one intrusive singly-linked list whose head is the static `allocated`. Only `args[0..filled-1]` hold live values; the tail is uninitialized.
- `closure_of` validates a value by scanning that list; `mmml_apply` copies arguments and calls `mmml_code`; `mmml_shutdown` frees the whole list.
- Values are 64-bit tagged words: ints `(n << 1) | 1`, `false=2`, `true=6`, `unit=10`, closures are non-zero 8-aligned pointers; `0` is not a value.
- The code generator spills every live `mmml_value` into a frame slot before each runtime `call` and uses only caller-saved registers (`a0/a1/t0`); top-level values live in `mmml_program_main`'s frame for the whole program.
- Convention is the RV64 psABI C calling convention; programs run under qemu-riscv64.

## Goals / Non-Goals

**Goals:**

- Non-moving mark-sweep collection that requires zero changes to generated assembly.
- A `mmml_collect(void)` entry point callable from assembly, plus automatic triggering in `mmml_make_closure`.
- Deterministic collection for tests; QEMU end-to-end proof with generated code.
- Keep C under `-Wall -Wextra -Werror -pedantic` and the change confined to `MMML/runtime/`.

**Non-Goals:**

- Moving/compacting collection and precise root tables (would couple the collector to the codegen's frame layout).
- Heap objects beyond closures (tuples/lists do not exist yet).
- Tail-call protocol, stack-overflow handling, and the legacy LLVM demo (`MMML/bin/runtime.c`) are untouched.

## Decisions

### 1. Conservative, non-moving mark-sweep

Collector marks from conservative roots on the stack and sweeps the existing `allocated` list in place. Alternatives:

- *Precise roots + moving (copying/compacting)*: needs frame descriptors emitted by the codegen, pointer updates in slots and objects, forwarding pointers, and changes to `closure_of`. Too much coupling to parallel work in the backend; rejected.
- *Precise roots + non-moving*: needs an explicit root registry and codegen cooperation; the codegen already stores every live value in slots, so precision buys little. Rejected.

Non-moving keeps `closure_of` validation and `mmml_apply` semantics unchanged and lets the intrusive list double as the sweep enumeration.

### 2. Roots: scan the stack between `sp` and a recorded stack base

`startup.c` records a stack base (address of a local in `main`) into a runtime global. `mmml_collect` scans every 8-aligned word in `[current_sp, stack_base)`; a word is a root if it is a member of the current allocation set. Membership uses a hash set of allocated closure addresses built once per collection, so each candidate is O(1) instead of an O(n) list walk. The scan naturally covers all generated frames (whose slots sit below the C frames), C locals, and the collector's own frame. Registers are not scanned: the generated-code convention keeps live values in slots, and the C compiler spills its own roots onto the C stack.

### 3. Trigger: allocation counter with auto-collect, plus explicit entry

The runtime tracks bytes allocated since the last collection. `mmml_make_closure` checks the counter *before* `malloc`; above the threshold (start: 1 MiB, see Open Questions) it runs a collection first. `mmml_collect` remains available for tests and optional explicit calls from generated code. No change to `mmml_make_closure`'s observable behavior.

### 4. Mark storage: explicit field, not a stolen bit

Add `bool marked;` to `mmml_closure` (one extra word after padding) instead of borrowing the high bit of `arity`. Clearer, robust against future object shapes, and trivially reset during sweep.

### 5. Trace only filled slots

Mark propagation reads exactly `args[0..filled-1]`; unfilled slots are uninitialized and must never be read (matches the spec scenario).

### 6. Full-application buffer becomes stack memory

Replace the `malloc`/`free` argument buffer in `mmml_apply` with `alloca(arity * sizeof(mmml_value))`. The buffer then lives on the C stack and is covered by the conservative scan, so captured closure arguments survive a collection triggered inside the callee. Alternative considered: an explicit temporary-root stack in the collector (more code, same effect); `alloca` is available on riscv64 glibc and is freed automatically on return.

### 7. Sweep reuses the allocation list

Sweep walks `allocated` with a previous/current pair: unmarked closures are unlinked and `free`d, survivors get `marked = false`. Because freed objects leave the list, `closure_of` keeps rejecting stale pointers without changes.

### 8. Layout: new `gc.c`, contract in `runtime.h`

The collector lives in `MMML/runtime/gc.c` (with a private `gc.h` only if the split needs it); `runtime.h` gains the `mmml_collect` declaration and a comment block stating the root contract for generated code (live values in stack slots; argument and caller-saved registers are not roots). `runtime.c` changes stay minimal (mark field init, byte counter, `alloca`, hook call).

## Risks / Trade-offs

- *Conservative scanning retains garbage (false positives from stale slot values)* → only wasted memory, never dangling pointers; acceptable, and the threshold keeps collections frequent enough to reuse address space.
- *A wrong scan bound would miss roots and free live closures* → the base is recorded in the outermost frame before any generated code runs; the QEMU integration test forces a collection with live values spread over several frames.
- *Freed addresses reused by later allocations weaken stale-pointer rejection* → documented limitation inherent to address reuse; the spec scenario covers the not-yet-reused case.
- *`alloca` is non-standard but supported by the target toolchain* → fallback is an explicit temp-root list in the collector if the RV64 build objects.
- *Collection pause is O(heap + stack)* → fine at course scale; the hash set keeps candidate checks O(1).
- *`closure_of` remains O(n) per apply* → pre-existing, out of scope; noted as a possible later optimization.

## Migration Plan

No migration: the runtime is a Task 3 prototype. Implementation branches from `origin/maltsev/codegen` (44af9b8, superset of `svmk17/runtime`). Verification: `dune build -p MMML @MMML/runtime/runtest --force`, then the cross-compiled `riscv64-linux-gnu-gcc` + `qemu-riscv64` test with a tiny threshold to force collections, then a full CPS factorial/fibonacci run through `mmml-asmgen` output once the backend is wired.

## Open Questions

- Exact threshold value: start at 1 MiB; tune after the benchmark. Does not affect specs or task breakdown.
- Whether the codegen adds explicit `mmml_collect` calls: not required by this design; the automatic trigger suffices.
