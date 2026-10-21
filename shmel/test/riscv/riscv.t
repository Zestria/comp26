Code for function bodies (task 3, part 1). Parameters come on the stack
(cdecl): the first one is at 0(s0), the second at 8(s0), ... The result of
every computation is in a0, operands are loaded into t0 and t1.

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
    ld t0, 0(s0)
    ld t1, 8(s0)
    add a0, t0, t1
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
    ld t0, 0(s0)
    addi a0, t0, 1
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
    ld t0, 0(s0)
    addi a0, t0, -2048
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
    ld t0, 0(s0)
    li t1, 100000
    add a0, t0, t1
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
    ld t0, 0(s0)
    neg a0, t0
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
    ld t0, 0(s0)
    ld t1, 8(s0)
    add a0, t0, t1
    sd a0, -24(s0)
    ld t0, 8(s0)
    ld t1, 16(s0)
    sub a0, t0, t1
    ld t0, -24(s0)
    mul a0, t0, a0
    ld t1, 0(s0)
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
    ld t0, 0(s0)
    li t1, 2
    bge t0, t1, .Lfac_base_else0
    li a0, 1
    j .Lfac_base_end0
  .Lfac_base_else0:
    ld t0, 0(s0)
    li t1, 10
    mul a0, t0, t1
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
    ld t0, 0(s0)
    ld t1, 8(s0)
    bgt t0, t1, .Lmin_else0
    ld a0, 0(s0)
    j .Lmin_end0
  .Lmin_else0:
    ld a0, 8(s0)
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
    ld t0, 0(s0)
    bnez t0, .Lis_zero_else0
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
    ld t0, 0(s0)
    ld t1, 8(s0)
    slt a0, t1, t0
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
    ld t0, 0(s0)
    seqz a0, t0
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
    ld t0, 0(s0)
    li t1, 3
    slt a0, t1, t0
    sd a0, -24(s0)
    beqz a0, .Lflag_else0
    j .Lflag_end0
  .Lflag_else0:
    ld t0, 0(s0)
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
    ld t0, 0(s0)
    addi a0, t0, 1
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
    ld t0, 0(s0)
    ld t1, 8(s0)
    bge t0, t1, .Lg_else0
    ld a0, 0(s0)
    j .Lg_end0
  .Lg_else0:
    ld a0, 8(s0)
  .Lg_end0:
    li t1, 2
    mul a0, a0, t1
    addi a0, a0, 1
    addi sp, s0, -16
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret
  

Calls, functions as values and local let rec are left to the other parts of
the backend (calls, closures, lambda lifting), here they are errors:

  $ echo "let f x = g (x + 1)" | ./riscv_tests.exe asm
  f: function calls are not supported here
  $ echo "let f x = let g = fun y -> x + y in g 1 + g 2" | ./riscv_tests.exe asm
  f: fun must be lifted before code generation
  $ echo "let f x = let rec g n = if n < 1 then 0 else g (n - 1) in g x" | ./riscv_tests.exe asm
  f: local let rec g must be lifted before code generation
