Code for function bodies (task 3, part 1). Calling convention: the standard
one of RISC-V (psABI). Parameters come in a0 .. a7 (the ninth and later on the
stack at 0(s0), 8(s0), ...), the result goes to a0. The result of every
computation is in a0, operands are loaded into t0 and t1.

Arithmetic; small constants go into the instruction itself:

  $ cat > arith.ml <<'EOF'
  > let add a b = a + b
  > let inc a = a + 1
  > let dec a = a - 2048
  > let big a = a + 100000
  > let neg a = - a
  > let calc a b c = (a + b) * (b - c) / a
  > EOF
  $ ./riscv_tests.exe asm < arith.ml
  add:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    add a0, a0, a1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  inc:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi a0, a0, 1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  dec:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi a0, a0, -2048
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  big:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    li t1, 100000
    add a0, a0, t1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  neg:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    neg a0, a0
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  calc:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    add a0, a0, a1
    sd a0, -32(s0)
    sub a0, a1, a2
    ld t0, -32(s0)
    mul a0, t0, a0
    ld t1, -24(s0)
    div a0, a0, t1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

A comparison in an if becomes one branch to the else part; the result of
both branches ends up in a0:

  $ cat > branches.ml <<'EOF'
  > let fac_base n = if n < 2 then 1 else n * 10
  > let min a b = if a <= b then a else b
  > let is_zero a = if a = 0 then 1 else 0
  > EOF
  $ ./riscv_tests.exe asm < branches.ml
  fac_base:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    li t1, 2
    bge a0, t1, .Lfac_base_else0
    li a0, 1
    j .Lfac_base_end0
  .Lfac_base_else0:
    li t1, 10
    mul a0, a0, t1
  .Lfac_base_end0:
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  min:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    bgt a0, a1, .Lmin_else0
    j .Lmin_end0
  .Lmin_else0:
    mv a0, a1
  .Lmin_end0:
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  is_zero:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    bnez a0, .Lis_zero_else0
    li a0, 1
    j .Lis_zero_end0
  .Lis_zero_else0:
    li a0, 0
  .Lis_zero_end0:
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

Comparisons as values: 0 or 1. A boolean variable is tested with beqz:

  $ cat > booleans.ml <<'EOF'
  > let le a b = a <= b
  > let eq0 a = a = 0
  > let flag a = let c = a > 3 in if c then c else a < 0
  > EOF
  $ ./riscv_tests.exe asm < booleans.ml
  le:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    slt a0, a1, a0
    xori a0, a0, 1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  eq0:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    seqz a0, a0
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  flag:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    li t1, 3
    slt a0, t1, a0
    sd a0, -32(s0)
    beqz a0, .Lflag_else0
    j .Lflag_end0
  .Lflag_else0:
    ld t0, -24(s0)
    slti a0, t0, 0
  .Lflag_end0:
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

A variable read only by the next computation stays in a0; other variables
get stack slots -24(s0), -32(s0), ... (here x):

  $ cat > lets.ml <<'EOF'
  > let m a = let x = a + 1 in let y = x * x in y - x
  > let g a b = let x = (if a < b then a else b) in x * 2 + 1
  > EOF
  $ ./riscv_tests.exe asm < lets.ml
  m:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    addi a0, a0, 1
    sd a0, -24(s0)
    mul a0, a0, a0
    ld t1, -24(s0)
    sub a0, a0, t1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  g:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    bge a0, a1, .Lg_else0
    j .Lg_end0
  .Lg_else0:
    mv a0, a1
  .Lg_end0:
    li t1, 2
    mul a0, a0, t1
    addi a0, a0, 1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

Parameters are read straight from their registers: a1 .. a7 until the first
call, a0 until the first computation. Only the parameters needed later are
saved to stack slots when the function starts:

  $ cat > params.ml <<'EOF'
  > let sub a b = b - a
  > let later a b = let x = b * 2 in x + a
  > let ninth a b c d e g h i j = i * j
  > EOF
  $ ./riscv_tests.exe asm < params.ml
  sub:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    sub a0, a1, a0
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  later:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    li t1, 2
    mul a0, a1, t1
    ld t1, -24(s0)
    add a0, a0, t1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  ninth:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    ld t1, 0(s0)
    mul a0, a7, t1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

A call (here of ext1 and ext2, the functions of the tests) spoils a* and t*,
so b is saved before it; the arguments are put into a0, a1 from the stack
slots:

  $ cat > calls.ml <<'EOF'
  > let after_call a b = let x = ext1 a in x + b
  > let two a b = ext2 b (a + 1)
  > EOF
  $ ./riscv_tests.exe asm < calls.ml
  after_call:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a0, -24(s0)
    sd a1, -32(s0)
    ld a0, -24(s0)
    call ext1
    ld t1, -32(s0)
    add a0, a0, t1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  
  two:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    addi sp, sp, -16
    sd a1, -24(s0)
    addi a0, a0, 1
    sd a0, -32(s0)
    ld a0, -24(s0)
    ld a1, -32(s0)
    call ext2
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

Functions as values and local let rec are left to closure conversion and
lambda lifting; here they are errors:

  $ echo "let f x = let g = fun y -> x + y in g 1 + g 2" | ./riscv_tests.exe asm
  f: fun must be lifted before code generation
  $ echo "let f x = let rec g n = if n < 1 then 0 else g (n - 1) in g x" | ./riscv_tests.exe asm
  f: local let rec g must be lifted before code generation
