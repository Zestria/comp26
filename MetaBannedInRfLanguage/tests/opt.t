Tagged constants fit addi only after multiplication by two.
Check both sides of the signed 12-bit boundary, including subtraction.
A constant on the left of addition uses the same immediate instruction.

  $ cat > immediates.ml <<EOF
  > let upper x = x + 1023;;
  > let commuted x = 1023 + x;;
  > let above x = x + 1024;;
  > let lower x = x + (-1024);;
  > let below x = x + (-1025);;
  > let subtract_lower x = x - 1024;;
  > let subtract_above x = x - (-1024)
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o immediates.s immediates.ml
  $ sed -n '/^  addi a0,/p; /^  li t1,/p; /^  add a0,/p; /^  sub a0,/p' immediates.s
    addi a0, a0, 2046
    addi a0, a0, 2046
    li t1, 2049
    add a0, t0, t1
    addi a0, a0, -1
    addi a0, a0, -2048
    li t1, -2049
    add a0, t0, t1
    addi a0, a0, -1
    addi a0, a0, -2048
    li t1, -2047
    sub a0, t0, t1
    addi a0, a0, 1

Unary plus, adding/subtracting zero, and dividing by one need no arithmetic
or closure allocation, even when combined in one expression.

  $ cat > identity.ml <<EOF
  > let identity x = +((0 + x) - 0) / 1
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o identity.s identity.ml
  $ grep -E '^  (add |div |call |slli |srai )' identity.s
  [1]

Division by -1 becomes tagged negation. A known nonzero divisor needs no
division-by-zero handler; division by a parameter still needs it.

  $ cat > division.ml <<EOF
  > let negate x = x / (-1);;
  > let third x = x / 3;;
  > let quotient x y = x / y
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o division.s division.ml
  $ grep -E '^  (sub a0,|addi a0,|div |call division_by_zero)' division.s
    sub a0, x0, a0
    addi a0, a0, 2
    div a0, t0, t1
    addi a0, a0, 1
    call division_by_zero
    div a0, t0, t1
    addi a0, a0, 1

Two references to (+) share one wrapper. The wrapper has five instructions,
with no frame, parameter spills, or redundant register moves.

  $ cat > wrappers.ml <<EOF
  > let choose b = if b then (+) else (+)
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o wrappers.s wrappers.ml
  $ grep -c '^  add a0, t0, t1$' wrappers.s
  1
  $ sed '/^  # /d' wrappers.s | tail -n 5
    ld t0, 0(a0)
    ld t1, 8(a0)
    add a0, t0, t1
    addi a0, a0, -1
    ret

Recursive calls also use the known entry point without allocating a closure
on each recursive step.

  $ cat > recursive.ml <<EOF
  > let rec sum n = if n = 0 then 0 else n + sum (n - 1)
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o recursive.s recursive.ml
  $ grep '^  call ' recursive.s | sed -E 's/\.L[0-9]+/<function>/g'
    call <function>
  $ grep -c '^  ret$' recursive.s
  2

A known partial application needs just one closure allocation. It does not
allocate an empty closure first and then invoke the generic apply helper.

  $ cat > partial.ml <<EOF
  > let add x y = x + y;;
  > let inc = add 1
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o partial.s partial.ml
  $ grep '^  call ' partial.s
    call closure_alloc

Constant conditions disappear in the compilation pipeline. Neither branches,
unconditional jumps, nor the unreachable division-by-zero code reach assembly.
Only the selected branches' additions of +2 and -2 to the tagged value remain.

  $ cat > constant_branches.ml <<EOF
  > let yes x = if true then x + 1 else x / 0;;
  > let no x = if false then x / 0 else x - 1
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o constant_branches.s constant_branches.ml
  $ grep -E '^  (addi a0,|beq |bne |blt |bge |j |div |call )' constant_branches.s
    addi a0, a0, 2
    addi a0, a0, -2

Inspect complete wrapper instruction sequences, excluding main, generated labels,
and explanatory comments.
These programs contain exactly one wrapper after main's ret, so extra frame
instructions, spills, moves, or helper calls would all appear in the output.

Unary plus only transfers its argument to a0. Negation and not each need
two arithmetic instructions on the tagged value, with no decoding or branches.

  $ for op in '~+' '~-' 'not'; do
  >   printf 'let op = (%s)\n' "$op" > unary.ml
  >   ../driver/compiler.exe --stop-after riscv -o unary.s unary.ml || break
  >   printf '%s:\n' "$op"
  >   sed '1,/^  ret$/d; /^\.L[0-9][0-9]*:$/d; /^  # /d' unary.s
  > done
  ~+:
    ld t0, 0(a0)
    mv a0, t0
    ret
  ~-:
    ld t0, 0(a0)
    sub a0, x0, t0
    addi a0, a0, 2
    ret
  not:
    ld t0, 0(a0)
    sub a0, x0, t0
    addi a0, a0, 4
    ret

Comparisons work directly on tagged integers. Greater-than reverses slt's
operands without moving registers. Equality uses xor/seqz without branching.
All three return a tagged boolean without allocating memory.

  $ for op in '<' '>' '='; do
  >   printf 'let op = (%s)\n' "$op" > comparison.ml
  >   ../driver/compiler.exe --stop-after riscv -o comparison.s comparison.ml || break
  >   printf '%s:\n' "$op"
  >   sed '1,/^  ret$/d; /^\.L[0-9][0-9]*:$/d; /^  # /d' comparison.s
  > done
  <:
    ld t0, 0(a0)
    ld t1, 8(a0)
    slt a0, t0, t1
    slli a0, a0, 1
    addi a0, a0, 1
    ret
  >:
    ld t0, 0(a0)
    ld t1, 8(a0)
    slt a0, t1, t0
    slli a0, a0, 1
    addi a0, a0, 1
    ret
  =:
    ld t0, 0(a0)
    ld t1, 8(a0)
    xor a0, t0, t1
    seqz a0, a0
    slli a0, a0, 1
    addi a0, a0, 1
    ret

As first-class functions, (&&) and (||) receive already evaluated arguments.
Their tagged booleans need just one and/or: no decoding, retagging, or branches.
Source-level short-circuit semantics are checked in ops.t and codegen.t.

  $ for op in '&&' '||'; do
  >   printf 'let op = (%s)\n' "$op" > boolean.ml
  >   ../driver/compiler.exe --stop-after riscv -o boolean.s boolean.ml || break
  >   printf '%s:\n' "$op"
  >   sed '1,/^  ret$/d; /^\.L[0-9][0-9]*:$/d; /^  # /d' boolean.s
  > done
  &&:
    ld t0, 0(a0)
    ld t1, 8(a0)
    and a0, t0, t1
    ret
  ||:
    ld t0, 0(a0)
    ld t1, 8(a0)
    or a0, t0, t1
    ret

Multiplication decodes the two temporary registers in place and immediately
tags its result in a0. Even this wrapper has no frame or runtime arithmetic call.

  $ printf 'let op = ( * )\n' > multiplication.ml
  $ ../driver/compiler.exe --stop-after riscv -o multiplication.s multiplication.ml
  $ sed '1,/^  ret$/d; /^\.L[0-9][0-9]*:$/d; /^  # /d' multiplication.s
    ld t0, 0(a0)
    ld t1, 8(a0)
    srai t0, t0, 1
    srai t1, t1, 1
    mul a0, t0, t1
    slli a0, a0, 1
    addi a0, a0, 1
    ret

A direct print_int call uses the native C ABI. It needs no ML argument
buffer, closure, apply helper, or print_int_closure adapter.
The only stack adjustments are the frames of main and output itself.

  $ cat > native_print.ml <<EOF
  > let output n = print_int n
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o native_print.s native_print.ml
  $ grep '^  call ' native_print.s
    call print_int
  $ grep '^  addi sp, sp,' native_print.s
    addi sp, sp, -16
    addi sp, sp, 16
    addi sp, sp, -32
    addi sp, sp, 32

End-to-end simplification: ANF folds constants, substitutes the alias, and
drops the unused pure multiplication. Codegen then uses a single addi.
Compare the entire assembly against the minimal source, so extra instructions
or extra local stack slots cannot be hidden by filtering the output.

  $ cat > simplified.ml <<EOF
  > let f x =
  >   let unused = x * x in
  >   let k = (2 + 3) * (9 - 1) in
  >   let copy = x in
  >   copy + k
  > EOF
  $ printf 'let f x = x + 40\n' > minimal.ml
  $ ../driver/compiler.exe --stop-after riscv -o simplified.s simplified.ml
  $ ../driver/compiler.exe --stop-after riscv -o minimal.s minimal.ml
  $ diff -u minimal.s simplified.s
  $ grep '^  addi a0,' simplified.s
    addi a0, a0, 80
