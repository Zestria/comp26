# Proposal

## Why

The MMML runtime (Task 3, RISC-V) allocates every closure with `malloc` and releases nothing until `mmml_shutdown`: long-running CPS programs consume memory proportional to the number of created closures. The code generator needs a GC entry point it can call from generated assembly, and the runtime must reclaim unreachable closures safely.

## What Changes

- Add a conservative, non-moving mark-sweep garbage collector to the MMML C runtime: closures are marked from conservative roots on the stack and swept over the existing intrusive allocation list.
- Add a public `mmml_collect(void)` entry point in `runtime.h` callable from generated assembly and from tests.
- Trigger collection automatically inside `mmml_make_closure` when a heap-usage threshold is exceeded; the threshold is derived from bytes allocated since the last collection.
- Trace only `args[0..filled-1]` of each closure (unfilled slots are uninitialized and must never be read by the collector).
- Fix the full-application path of `mmml_apply` so the transient argument buffer is visible to the collector (currently a `malloc`-ed buffer whose closure pointers would dangle if a callee triggers a collection).
- Record a stack base at initialization so the collector can bound its conservative scan.
- Keep the collector non-moving: closure addresses stay stable, so `closure_of` validation and `mmml_apply` semantics do not change.
- Add C-level GC tests (liveness, dangling-pointer rejection, collection triggered from nested allocation) and an RV64 QEMU integration test.
- Document the GC contract (root protocol for generated code, threshold policy, limitations) in `MMML/runtime/README.md`.

## Capabilities

### New Capabilities

- `mmml/runtime-gc`: automatic reclamation of unreachable closures in the MMML runtime, including the collection entry point, the root protocol the generated code must honor, and the invariant that collection never frees a reachable value.

### Modified Capabilities

None. The project currently has no main capability specs.

## Impact

- New `MMML/runtime/gc.c` with the collector; `MMML/runtime/runtime.h` gains `mmml_collect` and the GC contract.
- `MMML/runtime/runtime.c`: mark bookkeeping on closures, allocation accounting, auto-trigger, and the `mmml_apply` buffer fix.
- `MMML/runtime/startup.c`: record the stack base for the scan.
- `MMML/runtime/dune` and test files under `MMML/runtime/` gain GC tests, including a cross-compiled RV64 QEMU test.
- No changes to the generated assembly or the OCaml backend are required by this design; the codegen may optionally call `mmml_collect`.
- This change builds on the Task 3 branches (`origin/maltsev/codegen` at 44af9b8 supersets `origin/svmk17/runtime`) and does not touch the OCaml frontend or middleend.
