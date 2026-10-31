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

Peephole reuses the register saved by the immediately preceding stack store.
The store itself remains; returning the parameter needs no reload.

  $ grep -A2 '^  ld t0, 0(a0)$' identity.s
    ld t0, 0(a0)
    sd t0, -24(fp)
    mv a0, t0

Squaring a parameter originally reloads its stack slot into both t0 and t1.
The first load disappears because the store's source is already t0. This
exposes the second load to the same store, so it becomes a single move.
The spill stays in place while both copies are prepared for multiplication.

  $ printf 'let square x = x * x\n' > square.ml
  $ ../driver/compiler.exe --stop-after riscv -o square.s square.ml
  $ grep -A4 '^  sd t0, -24(fp)$' square.s
    sd t0, -24(fp)
    mv t1, t0
    srai t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1

A computed value is stored from a0. Its immediately following use in t0
becomes a move. The later load into t1 remains: this is a local rewrite.

  $ printf 'let fourth x = let squared = x * x in squared * squared\n' > fourth.ml
  $ ../driver/compiler.exe --stop-after riscv -o fourth.s fourth.ml
  $ grep -A4 '^  sd a0, -32(fp)$' fourth.s
    sd a0, -32(fp)
    mv t0, a0
    ld t1, -32(fp)
    srai t0, t0, 1
    addi t1, t1, -1

Different stack slots must not be confused. After spilling the second
parameter, returning the first still needs its own load. Returning the
second parameter can reuse t0. Both stores remain.

  $ cat > slots.ml <<EOF
  > let first x y = x;;
  > let second x y = y
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o slots.s slots.ml
  $ sed -n '/^  sd t0, -32(fp)$/,+1p' slots.s
    sd t0, -32(fp)
    ld a0, -24(fp)
    sd t0, -32(fp)
    mv a0, t0

Passing a freshly computed value to print_int needs no reload or mv a0, a0.
After the call the saved value must be loaded again: a0 now contains unit.
Keeping the original store is necessary for that later use.

  $ printf 'let save x = let y = x * x in let () = print_int y in y\n' > saved.ml
  $ ../driver/compiler.exe --stop-after riscv -o saved.s saved.ml
  $ grep -A5 '^  sd a0, -32(fp)$' saved.s
    sd a0, -32(fp)
    call print_int
    # print_int returns void; ML result is tagged unit
    li a0, 1
    sd a0, -40(fp)
    ld a0, -32(fp)

An intervening store prevents forwarding even within the argument buffer.
Structural equality saves both operands before loading the C arguments.

  $ printf 'let same x y = x = y\n' > heap_equality.ml
  $ ../driver/compiler.exe --stop-after riscv -o heap_equality.s heap_equality.ml
  $ grep -A3 '^  sd t0, 0(sp)$' heap_equality.s
    sd t0, 0(sp)
    sd t1, 8(sp)
    ld a0, 0(sp)
    ld a1, 8(sp)

The jump at the end of the then branch must skip the else branch.
The next label is its alternative entry, not the jump's destination.
Both branches retain their own stack loads.

  $ printf 'let choose flag x y = if flag then x else y\n' > branches.ml
  $ ../driver/compiler.exe --stop-after riscv -o branches.s branches.ml
  $ sed -n '/^  beq /,/^  ld ra,/p' branches.s | sed '$d'
    beq a0, t0, .L1
    ld a0, -32(fp)
    j .L2
  .L1:
    ld a0, -40(fp)
  .L2:

Exclusive branches reuse local slots. The longer then branch needs two
slots below the two parameters; the else branch reuses the first slot.
The function's frame is 48 bytes rather than 64.

  $ cat > branch_slots.ml <<EOF
  > let choose flag x =
  >   if flag then
  >     let a = x * x in let b = a * x in b + x
  >   else
  >     let c = x * x in c - x
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o branch_slots.s branch_slots.ml
  $ grep -E '^  addi sp, sp, -|^  sd a0, -' branch_slots.s
    addi sp, sp, -16
    addi sp, sp, -48
    sd a0, -40(fp)
    sd a0, -48(fp)
    sd a0, -40(fp)

When else needs more slots, its deepest offset determines the frame size.
The outer value stays at fp-40. Bindings after the join start below both
branches, at fp-72 and fp-80. This frame shrinks from 96 to 80 bytes.

  $ cat > branch_join.ml <<EOF
  > let join flag x =
  >   let outer = x + 1 in
  >   let y = if flag then
  >     let a = x * x in a + outer
  >   else
  >     let a = x * x in let b = a * x in let c = b * x in c + outer
  >   in
  >   let z = y * y in z + outer
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o branch_join.s branch_join.ml
  $ grep -E '^  addi sp, sp, -|^  sd a0, -' branch_join.s
    addi sp, sp, -16
    addi sp, sp, -80
    sd a0, -40(fp)
    sd a0, -48(fp)
    sd a0, -48(fp)
    sd a0, -56(fp)
    sd a0, -64(fp)
    sd a0, -72(fp)
    sd a0, -80(fp)

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
operands without moving registers.
Both return a tagged boolean without allocating memory.

  $ for op in '<' '>'; do
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

Multiplication uses tag(x*y) = x * (tag(y) - 1) + 1: four arithmetic
instructions, with no final shift, frame, or runtime arithmetic call.

  $ printf 'let op = ( * )\n' > multiplication.ml
  $ ../driver/compiler.exe --stop-after riscv -o multiplication.s multiplication.ml
  $ sed '1,/^  ret$/d; /^\.L[0-9][0-9]*:$/d; /^  # /d' multiplication.s
    ld t0, 0(a0)
    ld t1, 8(a0)
    srai t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1
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

Known immediate operands keep scalar equality branch-free, even though (=)
also supports heap tuples. Neither case calls the structural equality helper.

  $ cat > scalar_equality.ml <<EOF
  > let zero x = x = 0;;
  > let nonzero x = x <> 0
  > EOF
  $ ../driver/compiler.exe --stop-after riscv -o scalar_equality.s scalar_equality.ml
  $ grep -E '^  (xor |seqz |snez |slli |addi a0,|beq |bne |call )' scalar_equality.s
    xor a0, t0, t1
    seqz a0, a0
    slli a0, a0, 1
    addi a0, a0, 1
    xor a0, t0, t1
    snez a0, a0
    slli a0, a0, 1
    addi a0, a0, 1

Tuple projection uses direct loads only for the requested fields. The first
load reads the tuple argument; the next two read fields 1 and 3 after its header.

  $ printf 'let keep (_, x, _, y) = x + y\n' > projection.ml
  $ ../driver/compiler.exe --stop-after riscv -o projection.s projection.ml
  $ grep -E '^  ld t0, [0-9]+\(a0\)$' projection.s
    ld t0, 0(a0)
    ld t0, 16(a0)
    ld t0, 32(a0)
  $ grep '^  call ' projection.s
  [1]

ANF can eliminate a locally constructed tuple whose fields are immediately
destructured. The full assembly is identical to scalar addition: no allocation.

  $ printf 'let f x = let (a, b) = (x, 40) in a + b\n' > local_tuple.ml
  $ ../driver/compiler.exe --stop-after riscv -o local_tuple.s local_tuple.ml
  $ diff -u minimal.s local_tuple.s
