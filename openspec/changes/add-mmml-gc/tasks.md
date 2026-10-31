# Tasks

## 1. Base branch and preconditions

- [x] 1.1 Create a feature branch from `origin/maltsev/codegen` (44af9b8) and verify `dune build -p MMML @MMML/runtime/runtest --force` passes before any change
- [x] 1.2 Read `MMML/runtime/README.md` and confirm the runtime contract and test commands documented there still match the branch state

## 2. Collector core

- [x] 2.1 Add `bool marked;` to `mmml_closure` in `MMML/runtime/runtime.c` and set it to `false` in `mmml_make_closure`; verify the existing runtime tests still pass
- [x] 2.2 Create `MMML/runtime/gc.c` with the allocation hash set, the stack scan between the recorded base and the current stack pointer, and membership-based root marking; verify a C test that drops all references to a closure, collects, and observes it removed from the allocation list
- [x] 2.3 Implement mark propagation over `args[0..filled-1]` only; verify a C test where a live closure captures another closure keeps both alive across a collection, including one with unfilled slots
- [x] 2.4 Implement the sweep pass (unlink and free unmarked closures, reset marks on survivors); verify a C test that a pointer to a live closure stays valid and designates the same closure after collection

## 3. Runtime integration

- [x] 3.1 Record the stack base in `MMML/runtime/startup.c` (address of a local in `main`) into a runtime global and expose it to the collector; verify the collector's scan bound covers a C test with live values in nested frames
- [x] 3.2 Track bytes allocated since the last collection and trigger a collection in `mmml_make_closure` before `malloc` when the threshold is exceeded; verify a C test where creating closures past the threshold frees previously dropped closures
- [x] 3.3 Replace the `malloc`/`free` argument buffer in the full-application path of `mmml_apply` with `alloca`; verify a C test where a callee's allocations trigger a collection and the applied closure arguments remain intact
- [x] 3.4 Keep `closure_of` working after collections; verify a C test that applying a pointer to a collected (not yet reused) closure fails with the standard "invalid closure" error

## 4. Public contract and documentation

- [x] 4.1 Declare `void mmml_collect(void);` in `MMML/runtime/runtime.h` with the root-contract comment (live values in stack slots; argument and caller-saved registers are not roots); verify the header compiles cleanly as part of the runtime test build with `-Wall -Wextra -Werror -pedantic`
- [x] 4.2 Wire `gc.c` into `MMML/runtime/dune` (cram deps and test sources) so `dune build -p MMML @MMML/runtime/runtest --force` runs the collector tests
- [x] 4.3 Document the GC in `MMML/runtime/README.md`: entry point, threshold policy, root protocol for generated code, and the stale-pointer reuse limitation; verify every command shown in the updated README runs as written
- [x] 4.4 Add a C test for repeatable collection: two `mmml_collect` calls in a row free nothing live and leave the runtime functional; verify it passes in the dune runtest alias

## 5. RV64 QEMU integration

- [x] 5.1 Add a cross-compiled test that forces collections with a tiny threshold, allocates closures in nested frames, drops some, and checks survivors via `mmml_apply`; verify it passes with `riscv64-linux-gnu-gcc -static` plus `qemu-riscv64`
- [x] 5.2 Run a CPS factorial/fibonacci program against the runtime under qemu with the small threshold and verify results and clean exit; record the invocation in the runtime README

## 6. Final integration verification

- [x] 6.1 Verify `dune build -p MMML @check @runtest @install` passes from the repo root with the collector included
- [x] 6.2 Verify the generated-asm path still links against the updated runtime (compile the backend's asm output together with `runtime.c` and `startup.c`) and behaves identically with the default threshold

## Workflow follow-up

- Coordinate with the codegen owner whether to emit explicit `mmml_collect` calls; not required by this design.
- After review, archive the change following the project's process.
