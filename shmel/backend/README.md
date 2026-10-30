# RV64 closures

`Closure` describes the layout used by the backend. `closure_runtime.h`
defines the same layout for the future C runtime. A function value is an
untagged 64-bit pointer to a heap-allocated closure:

| Byte offset | Field | Meaning |
| --- | --- | --- |
| 0 | code | Address of the lifted function's code |
| 8 | arity | Total number of code parameters, including captures made explicit by closure conversion |
| 16 | applied | Number of arguments already saved |
| 24 + 8*i | arguments[i] | Saved argument i, in source order |

A function with no saved arguments still needs a 24-byte header. Only real
functions get closures; a top-level value with zero parameters is a value,
not a zero-arity function. Integer and boolean values occupy one word too.

Closures are immutable: partial application allocates a new object and copies
the existing arguments before appending the new ones. This prevents one use
of a function from changing another use of the same function. Runtime allocation
is implemented with `malloc`; there is no GC, and closures are not automatically
freed. Temporary arrays used to call saturated functions are freed after use.

For example, `helper k n m` has arity 3. The result of `helper k n` contains
the original code pointer, arity 3, applied count 2, and the words for `k`
and `n`. Its allocation size is 40 bytes. It still needs one argument.

`shmel_apply` combines saved and incoming arguments:

- Fewer than `arity`: return a new closure.
- Exactly `arity`: invoke the code with the combined arguments.
- More than `arity`: invoke the code, then apply the remaining arguments to
  its result, which must be a function in a well-typed program.

Stored closures satisfy `0 <= applied < arity`; a saturated closure is never
stored. The `code` field always refers to the original full-arity function,
not a partial-application trampoline.

## Calling convention

Generated functions and the C runtime both use the standard RV64 integer
psABI for word-sized values:

- Arguments 0 through 7 go in `a0` through `a7`.
- Argument 8 goes at `0(sp)` on entry, argument 9 at `8(sp)`, and so on.
- The caller allocates and releases stack arguments and keeps `sp` aligned
  to 16 bytes. Padding follows the stack arguments, not the first one.
- The result is one word in `a0`. Argument and temporary registers are
  caller-saved; `s0` through `s11` are callee-saved.

Saved and incoming arguments are combined in source order before assigning
registers or stack locations. There is no hidden environment or closure
parameter at the code entry. For example, applying `helper k n` to `m`
calls the original code with `k` in `a0`, `n` in `a1`, and `m` in `a2`.
More than eight parameters are allowed; saved arguments can also occupy
stack positions when the final application is made.

No bridge between different calling conventions is needed. `shmel_invoke`,
implemented in `closure_runtime.c`, validates the call and delegates to
`shmel_invoke_abi` in `closure_invoke.S`. The dispatcher copies the first eight
arguments to `a0` through `a7`, puts any remaining words on a 16-byte-aligned
stack, and calls the code address. It restores `sp`, `ra` and the callee-saved
registers it uses, preserving the result in `a0`.

All values, including encoded closure pointers, are passed as word bits rather
than as typed C pointers. The callee must follow this word-valued contract.
The dispatcher reads but does not modify the argument array. Invocation state
is kept on the stack, so nested applications do not overwrite another call's
state. There is no fixed arity limit or external FFI dependency; available
memory and stack space still limit the number of arguments.

The `shmel_code_address` typedef is a storage type, not the actual prototype
of every function. Calling it as a zero-argument function would be incorrect.

`Closure.t` is only compile-time metadata (a code label and total arity).
Creating it does not allocate a runtime closure. Runtime allocation, application
and `Calls.call` are implemented; global closure initialization remains a
separate whole-program integration step. Global function values must also point to valid
closure objects. A C builtin can supply its code address directly only when
its parameter and result types match the word-valued entry contract. Otherwise
it needs a wrapper (for example, to turn a `void` result into the language's
unit value), not a calling-convention bridge.

## Runtime API

```c
shmel_closure *shmel_make_closure(shmel_code_address code, uint64_t arity);
shmel_word shmel_apply(const shmel_closure *closure, uint64_t argc,
                       const shmel_word *argv);
```

`shmel_make_closure` creates the initial closure with no saved arguments.
`shmel_apply` allocates partial closures itself; callers do not need to build
partial-application objects manually. With zero new arguments, it returns the
same closure pointer encoded as a word. Neither routine modifies its inputs.
Contract violations and allocation failures print an error and abort.
Since words have no type tags, overapplication assumes that intermediate
results really are closure pointers; this must be guaranteed by the compiler.

For cross-compilation on Ubuntu, install the RISC-V toolchain and QEMU:

```sh
apt-get update
apt-get install gcc-riscv64-linux-gnu libc6-dev-riscv64-cross \
  qemu-user
```

Link both the C runtime and its RV64 dispatcher with the generated assembly:

```sh
riscv64-linux-gnu-gcc -static -march=rv64gc program.s \
  shmel/backend/closure_runtime.c shmel/backend/closure_invoke.S -o program
qemu-riscv64 -cpu max ./program
```

The generated program must provide its own entry point and initialize globals.
Use `Calls.hooks ~load_global` with `Codegen.compile_function` to generate closure
applications. The supplied loader must load values from initialized global
slots and preserve every register except its destination, as required by
`Codegen.hooks`. Function-valued slots contain closure pointers, not code labels.

`Calls.call` builds a temporary argument array on the stack, keeping it
16-byte aligned, and passes `(closure, argc, argv)` in `a0`, `a1`, `a2` to
`shmel_apply`. Operands are loaded from frame-pointer-relative slots or globals,
so filling the array does not overwrite later operands. Offsets and stack
adjustments beyond the 12-bit immediate range use a scratch register. On return,
the array is released without changing the result in `a0`.

Run the runtime tests with:

```sh
opam exec -- dune runtest shmel/test/riscv --force
```

On a RISC-V host the native runtime tests use the real dispatcher. Other hosts
use `closure_invoke_native.c`, a test-only C adapter covering the arities in
the native runtime suite; this adapter is not part of the production runtime.
When `riscv64-linux-gnu-gcc` and `qemu-riscv64` are available, the same tests
also run using the actual RV64 dispatcher. Tests include argument
order, independent and chained partial applications, overapplication, 8/9/10
arguments, and reentrant applications implementing CPS factorial/Fibonacci.
These CPS functions are written in C to isolate the runtime; they do not yet
test the compiler's full pipeline.

`calls_integration_tests.ml` separately checks the actual source-to-codegen
pipeline: parsing, ANF, closure conversion, lifting, `Calls.hooks`, then QEMU
with the RV64 dispatcher. A test-only C harness creates global closures, initializes values
and provides `print_int`; it is not a production implementation of whole-program
code generation. Results and output are compared against `Eval` on the original
source. Cases cover recursion, escaping closures, captured values, partial and
excess applications, argument order, preserved locals, CPS factorial/Fibonacci,
and 300-argument calls/partial applications with large stack offsets.

## Docker tests

From the repository root, run:

```sh
opam exec -- dune build @shmel/runtest-docker
```

This builds the OCaml test executable on the host, generates the same fixed
and random codegen cases as the local tests, and compares their RV64 results
with `Eval`. Cross-compilation and QEMU execution happen inside Docker. The
alias also runs the closure integration and runtime tests. Docker must be
running; missing Docker or failed container commands fail the tests rather
than skip them.

The toolchain image `shmel-riscv-tests:local` is built from
`shmel/test/riscv/Dockerfile`. The first run needs network access and installs
the cross-compiler and QEMU. Later runs reuse Docker's image
layers. Source changes do not invalidate the toolchain layer.

Local and Docker runners share `run_riscv.sh`, test generators, expected
results and comparison logic. Files are streamed into disposable containers;
no host source directory or Docker socket is mounted inside them. Set `DOCKER`
to the CLI's absolute path if it is not in PATH. `SHMEL_QEMU_TIMEOUT` controls
the execution timeout in seconds (default 60, using `timeout` where available).

The regular `dune runtest shmel/test` remains local and does not start Docker.
The Docker alias reruns on each invocation, even if the source is unchanged.
