End-to-end regression tests for the RISC-V 64 backend.  Every program is
compiled by the compiler, assembled and linked with riscv64-linux-gnu-gcc
against runtime/runtime.c and run under qemu-riscv64, each run bounded
by timeout --kill-after=1s 10 so a runaway program fails instead of
hanging, with both stdout and the exit code checked.  Expected outputs
were checked against the OCaml toplevel.  riscv64-linux-gnu-gcc,
qemu-riscv64 and coreutils timeout must be installed; the test fails
otherwise.

The first case also snapshots the assembly for a small CPS factorial.

  $ cat > fac.ml <<'EOF'
  > let rec fact n k = if n = 0 then k 1 else fact (n - 1) (fun v -> k (n * v));;
  > print_int (fact 5 (fun v -> v))
  > EOF
  $ ../compiler.exe fac.ml || exit $?
  	.text
  	.globl	ml_main
  ml_code_0:
  	addi	sp, sp, -48
  	sd	ra, 32(sp)
  	sd	s1, 8(sp)
  	sd	s2, 16(sp)
  	sd	s3, 24(sp)
  	ld	s1, 24(a0)
  	ld	s2, 32(a0)
  	mv	s3, a1
  	srai	t2, s1, 1
  	addi	t3, s3, -1
  	mul	t2, t2, t3
  	addi	s1, t2, 1
  	mv	a1, s1
  	mv	a0, s2
  	ld	t0, 16(a0)
  	li	t1, 1
  	bne	t0, t1, .L3
  	ld	t0, 8(a0)
  	jalr	ra, 0(t0)
  	j	.L4
  .L3:
  	sd	s1, 0(sp)
  	mv	a0, s2
  	li	a1, 1
  	mv	a2, sp
  	call	rt_apply
  .L4:
  	ld	ra, 32(sp)
  	ld	s1, 8(sp)
  	ld	s2, 16(sp)
  	ld	s3, 24(sp)
  	addi	sp, sp, 48
  	ret
  ml_code_1:
  	addi	sp, sp, -48
  	sd	ra, 32(sp)
  	sd	s1, 8(sp)
  	sd	s2, 16(sp)
  	sd	s3, 24(sp)
  	mv	s1, a1
  	mv	s2, a2
  	li	t1, 1
  	xor	t2, s1, t1
  	seqz	t2, t2
  	beqz	t2, .L5
  	li	a1, 3
  	mv	a0, s2
  	ld	t0, 16(a0)
  	li	t1, 1
  	bne	t0, t1, .L6
  	ld	t0, 8(a0)
  	jalr	ra, 0(t0)
  	j	.L7
  .L6:
  	li	t0, 3
  	sd	t0, 0(sp)
  	mv	a0, s2
  	li	a1, 1
  	mv	a2, sp
  	call	rt_apply
  .L7:
  	ld	ra, 32(sp)
  	ld	s1, 8(sp)
  	ld	s2, 16(sp)
  	ld	s3, 24(sp)
  	addi	sp, sp, 48
  	ret
  .L5:
  	li	a0, 5
  	call	rt_alloc
  	li	t0, 1
  	sd	t0, 0(a0)
  	la	t0, ml_code_0
  	sd	t0, 8(a0)
  	li	t0, 1
  	sd	t0, 16(a0)
  	sd	s1, 24(a0)
  	sd	s2, 32(a0)
  	mv	s2, a0
  	addi	s1, s1, -2
  	mv	a1, s1
  	mv	a2, s2
  	call	ml_code_1
  	ld	ra, 32(sp)
  	ld	s1, 8(sp)
  	ld	s2, 16(sp)
  	ld	s3, 24(sp)
  	addi	sp, sp, 48
  	ret
  ml_code_2:
  	addi	sp, sp, -16
  	sd	s1, 0(sp)
  	mv	s1, a1
  	mv	a0, s1
  	ld	s1, 0(sp)
  	addi	sp, sp, 16
  	ret
  ml_main:
  	addi	sp, sp, -16
  	sd	ra, 8(sp)
  	sd	s1, 0(sp)
  	la	s1, ml_code_2_closure
  	li	a1, 11
  	mv	a2, s1
  	call	ml_code_1
  	mv	s1, a0
  	mv	a0, s1
  	call	rt_print_int
  	li	a0, 1
  	ld	ra, 8(sp)
  	ld	s1, 0(sp)
  	addi	sp, sp, 16
  	ret
  	.data
  	.p2align	3
  ml_code_2_closure:
  	.quad	1
  	.quad	ml_code_2
  	.quad	1

  $ ../compiler.exe fac.ml -o fac.s || exit $?
  $ riscv64-linux-gnu-gcc -static fac.s ../runtime/runtime.c -o fac.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./fac.bin && echo " exit=0" || echo " exit=$?"
  120 exit=0

CPS factorial for several inputs.

  $ cat > cps_fact.ml <<'EOF'
  > let rec fact n k = if n = 0 then k 1 else fact (n - 1) (fun v -> k (n * v));;
  > print_int (fact 0 (fun v -> v));;
  > print_int (fact 1 (fun v -> v));;
  > print_int (fact 5 (fun v -> v));;
  > print_int (fact 10 (fun v -> v));;
  > print_int (fact 20 (fun v -> v))
  > EOF
  $ ../compiler.exe cps_fact.ml -o cps_fact.s || exit $?
  $ riscv64-linux-gnu-gcc -static cps_fact.s ../runtime/runtime.c -o cps_fact.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cps_fact.bin && echo " exit=0" || echo " exit=$?"
  1112036288002432902008176640000 exit=0

CPS fibonacci for several inputs.

  $ cat > cps_fib.ml <<'EOF'
  > let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)));;
  > print_int (fib 0 (fun v -> v));;
  > print_int (fib 1 (fun v -> v));;
  > print_int (fib 10 (fun v -> v));;
  > print_int (fib 20 (fun v -> v))
  > EOF
  $ ../compiler.exe cps_fib.ml -o cps_fib.s || exit $?
  $ riscv64-linux-gnu-gcc -static cps_fib.s ../runtime/runtime.c -o cps_fib.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cps_fib.bin && echo " exit=0" || echo " exit=$?"
  01556765 exit=0

Closures capture free variables, partial application returns a closure,
and over-application of a known-arity function applies the result again.

  $ cat > closures.ml <<'EOF'
  > let make a b c d = let s = a + b in let t = c + d in fun x -> s + t + x;;
  > let f = make 1 2 3 4;;
  > print_int (f 10);;
  > print_int ((fun x y -> x * 100 + y) 3 4);;
  > let add = (+);;
  > let inc = add 1;;
  > print_int (inc 41);;
  > let g x = fun y -> x + y;;
  > print_int (g 1 2);;
  > let twice h x = h (h x);;
  > print_int (twice (fun n -> n + 3) 4)
  > EOF
  $ ../compiler.exe closures.ml -o closures.s || exit $?
  $ riscv64-linux-gnu-gcc -static closures.s ../runtime/runtime.c -o closures.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./closures.bin && echo " exit=0" || echo " exit=$?"
  2030442310 exit=0

Functions and primitives are first-class values.

  $ cat > closure_as_value.ml <<'EOF'
  > let f = (fun x -> x + 1);;
  > print_int (f 41);;
  > let cmp = (<);;
  > if cmp 1 2 then print_int 1 else print_int 0
  > EOF
  $ ../compiler.exe closure_as_value.ml -o closure_as_value.s || exit $?
  $ riscv64-linux-gnu-gcc -static closure_as_value.s ../runtime/runtime.c -o closure_as_value.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./closure_as_value.bin && echo " exit=0" || echo " exit=$?"
  421 exit=0

A generic apply over-applies a nine-argument function by one argument.

  $ cat > over.ml <<'EOF'
  > let f9 a b c d e f g h i = fun j -> a + b + c + d + e + f + g + h + i + j;;
  > let apply h = h 1 2 3 4 5 6 7 8 9 10;;
  > print_int (apply f9)
  > EOF
  $ ../compiler.exe over.ml -o over.s || exit $?
  $ riscv64-linux-gnu-gcc -static over.s ../runtime/runtime.c -o over.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./over.bin && echo " exit=0" || echo " exit=$?"
  55 exit=0

A captured primitive is applied partially and destructured from a tuple.

  $ cat > prim_capture.ml <<'EOF'
  > let part f = fun x -> f x;;
  > print_int (part (+) 1 2);;
  > let t = ((+), 3) in print_int (let (f, x) = t in f x x)
  > EOF
  $ ../compiler.exe prim_capture.ml -o prim_capture.s || exit $?
  $ riscv64-linux-gnu-gcc -static prim_capture.s ../runtime/runtime.c -o prim_capture.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./prim_capture.bin && echo " exit=0" || echo " exit=$?"
  36 exit=0

Nine arguments: a direct call, a partial application and an unknown callee.

  $ cat > args9.ml <<'EOF'
  > let f9 a b c d e f g h i = a + b + c + d + e + f + g + h + i;;
  > print_int (f9 1 2 3 4 5 6 7 8 9);;
  > let p4 = f9 1 2 3 4;;
  > print_int (p4 5 6 7 8 9);;
  > let apply9 h = h 1 2 3 4 5 6 7 8 9;;
  > print_int (apply9 f9)
  > EOF
  $ ../compiler.exe args9.ml -o args9.s || exit $?
  $ riscv64-linux-gnu-gcc -static args9.s ../runtime/runtime.c -o args9.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./args9.bin && echo " exit=0" || echo " exit=$?"
  454545 exit=0

Twelve arguments: a direct call, a partial application and an unknown callee.

  $ cat > args12.ml <<'EOF'
  > let f12 a b c d e f g h i j k l = a + b + c + d + e + f + g + h + i + j + k + l;;
  > print_int (f12 1 2 3 4 5 6 7 8 9 10 11 12);;
  > let p = f12 1 2 3 4;;
  > print_int (p 5 6 7 8 9 10 11 12);;
  > let apply12 h = h 1 2 3 4 5 6 7 8 9 10 11 12;;
  > print_int (apply12 f12)
  > EOF
  $ ../compiler.exe args12.ml -o args12.s || exit $?
  $ riscv64-linux-gnu-gcc -static args12.s ../runtime/runtime.c -o args12.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./args12.bin && echo " exit=0" || echo " exit=$?"
  787878 exit=0

Sixteen constant arguments combined through nested direct calls.  The
constants fold into the literal arguments of each call, so only the
intermediate results stay live across calls and they fit in registers.

  $ cat > nested.ml <<'EOF'
  > let s x y = x + y;;
  > let a = 1 in
  > let b = 2 in
  > let c = 3 in
  > let d = 4 in
  > let e = 5 in
  > let f = 6 in
  > let g = 7 in
  > let h = 8 in
  > let i = 9 in
  > let j = 10 in
  > let k = 11 in
  > let l = 12 in
  > let m = 13 in
  > let n = 14 in
  > let o = 15 in
  > let p = 16 in
  > print_int (s (s (s (s a b) (s c d)) (s (s e f) (s g h))) (s (s (s (s i j) (s k l)) (s (s m n) (s o p))) 0))
  > EOF
  $ ../compiler.exe nested.ml -o nested.s || exit $?
  $ riscv64-linux-gnu-gcc -static nested.s ../runtime/runtime.c -o nested.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./nested.bin && echo " exit=0" || echo " exit=$?"
  136 exit=0

Twelve values computed at run time by a recursive function stay live
across calls inside both arms of a branch and are all consumed after the
join by a Horner-style weighted sum, so a permutation of the values
cannot hide; that is more than the eleven allocatable registers, so the
allocator spills the extras to stack slots.

  $ cat > branch_spill.ml <<'EOF'
  > let rec value x = if x = 0 then 0 else 1 + value (x - 1);;
  > let busy flag =
  >   let a = value 1 in
  >   let b = a + value 2 in
  >   let c = b + value 3 in
  >   let d = c + value 4 in
  >   let e = d + value 5 in
  >   let f = e + value 6 in
  >   let g = f + value 7 in
  >   let h = g + value 8 in
  >   let i = h + value 9 in
  >   let j = i + value 10 in
  >   let k = j + value 11 in
  >   let l = k + value 12 in
  >   let m =
  >     if flag
  >     then let u = value 13 in let v = u + value 14 in u + v
  >     else let w = value 15 in let z = w + value 16 in w + z
  >   in
  >   (((((((((((a * 3 + b) * 3 + c) * 3 + d) * 3 + e) * 3 + f) * 3 + g) * 3 + h) * 3 + i) * 3 + j) * 3 + k) * 3 + l) * 3 + m;;
  > print_int (busy true);;
  > print_int (busy false)
  > EOF
  $ ../compiler.exe branch_spill.ml -o branch_spill.s || exit $?
  $ riscv64-linux-gnu-gcc -static branch_spill.s ../runtime/runtime.c -o branch_spill.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./branch_spill.bin && echo " exit=0" || echo " exit=$?"
  17935061793512 exit=0

Runtime-computed arguments reach an unknown callee in a permuted order,
through an exact application, a partial application completed later and
an over-application; the weighted body makes the argument order affect
the result.

  $ cat > perm_apply.ml <<'EOF'
  > let rec value x = if x = 0 then 0 else 1 + value (x - 1);;
  > let w a b c d e f g h i j k l =
  >   a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9 + j * 10 + k * 11 + l * 12;;
  > let u a b c d e f g h i j k l = fun z ->
  >   a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9 + j * 10 + k * 11 + l * 12 + z * 13;;
  > let exact h = h (value 2) (value 9) (value 4) (value 11) (value 6) (value 1) (value 8) (value 12) (value 3) (value 10) (value 5) (value 7);;
  > let part h = h (value 1) (value 2) (value 3) (value 4) (value 5) (value 6) (value 7) (value 8);;
  > let overap h = h (value 2) (value 9) (value 4) (value 11) (value 6) (value 1) (value 8) (value 12) (value 3) (value 10) (value 5) (value 7) (value 13);;
  > print_int (exact w);;
  > print_int (part w (value 9) (value 10) (value 11) (value 12));;
  > print_int (overap u)
  > EOF
  $ ../compiler.exe perm_apply.ml -o perm_apply.s || exit $?
  $ riscv64-linux-gnu-gcc -static perm_apply.s ../runtime/runtime.c -o perm_apply.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./perm_apply.bin && echo " exit=0" || echo " exit=$?"
  530650699 exit=0

A closure capturing 300 runtime values; the last environment slots lie
beyond the 12-bit immediate range on both the capturing stores and the
callee's entry loads.  The program is generated by awk to keep this file
small.

  $ awk 'BEGIN {
  >   print "let rec value x = if x = 0 then 0 else 1 + value (x - 1);;";
  >   printf "let make";
  >   for (i = 1; i <= 300; i++) printf " a%d", i;
  >   printf " = fun y -> a1 * 1";
  >   for (i = 2; i <= 300; i++) printf " + a%d * %d", i, i;
  >   print " + y * 301;;";
  >   printf "let clo = make";
  >   for (i = 1; i <= 300; i++) printf " (value %d)", i;
  >   print ";;";
  >   print "print_int (clo (value 301))";
  > }' > env300.ml
  $ ../compiler.exe env300.ml -o env300.s || exit $?
  $ riscv64-linux-gnu-gcc -static env300.s ../runtime/runtime.c -o env300.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./env300.bin && echo " exit=0" || echo " exit=$?"
  9135651 exit=0

print_int reads a spilled value directly into its argument register and
travels through an unknown function parameter.

  $ cat > print_paths.ml <<'EOF'
  > let rec value x = if x = 0 then 0 else 1 + value (x - 1);;
  > let app f x = f x;;
  > let show s =
  >   let a = value 1 in
  >   let b = a + value 2 in
  >   let c = b + value 3 in
  >   let d = c + value 4 in
  >   let e = d + value 5 in
  >   let f = e + value 6 in
  >   let g = f + value 7 in
  >   let h = g + value 8 in
  >   let i = h + value 9 in
  >   let j = i + value 10 in
  >   let k = j + value 11 in
  >   let l = k + value 12 in
  >   let total = a + b + c + d + e + f + g + h + i + j + k + l in
  >   let p = print_int l in
  >   total * 1000 + l;;
  > print_int (show 0);;
  > app print_int (value 7)
  > EOF
  $ ../compiler.exe print_paths.ml -o print_paths.s || exit $?
  $ riscv64-linux-gnu-gcc -static print_paths.s ../runtime/runtime.c -o print_paths.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./print_paths.bin && echo " exit=0" || echo " exit=$?"
  783640787 exit=0

Every comparison branches directly on the comparison bit in tail
position, with the constant operand on the left and on the right,
including the signed bounds of the 63-bit range.  All sixteen fuse, so
counting beqz t2, the shape a fused comparison branches with, gives
seventeen sites together with the helper's own comparison; a
materialized boolean is instead tested with beq against the false
immediate, and no comparison in the file has two variable operands,
whose slow-path test would also be a beqz t2.

  $ cat > cmp_fusion.ml <<'EOF'
  > let rec value x = if x = 0 then 0 else 1 + value (x - 1);;
  > let lt_l t = if 1 < t then 10 else 20;;
  > let lt_r t = if t < 5 then 30 else 40;;
  > let le_l t = if 1 <= t then 50 else 60;;
  > let le_r t = if t <= 2 then 70 else 80;;
  > let gt_l t = if 5 > t then 90 else 100;;
  > let gt_r t = if t > 1 then 110 else 120;;
  > let eq_l t = if 3 = t then 130 else 140;;
  > let eq_r t = if t = 4 then 150 else 160;;
  > let ne_l t = if 3 <> t then 170 else 180;;
  > let ne_r t = if t <> 4 then 190 else 200;;
  > let ge_l t = if 3 >= t then 210 else 220;;
  > let ge_r t = if t >= 4 then 230 else 240;;
  > let bmax t = if t < 4611686018427387903 then 1 else 2;;
  > let bmin t = if t > (-4611686018427387904) then 3 else 4;;
  > let bemax t = if t = 4611686018427387903 then 5 else 6;;
  > let bemin t = if t = (-4611686018427387904) then 7 else 8;;
  > print_int (lt_l (value 3));;
  > print_int (lt_r (value 3));;
  > print_int (le_l (value 3));;
  > print_int (le_r (value 3));;
  > print_int (gt_l (value 3));;
  > print_int (gt_r (value 3));;
  > print_int (eq_l (value 3));;
  > print_int (eq_r (value 3));;
  > print_int (ne_l (value 3));;
  > print_int (ne_r (value 3));;
  > print_int (ge_l (value 3));;
  > print_int (ge_r (value 3));;
  > print_int (bmax (value 9));;
  > print_int (bmin (value 9));;
  > print_int (bemax 4611686018427387903);;
  > print_int (bemin (-4611686018427387904))
  > EOF
  $ ../compiler.exe cmp_fusion.ml -o cmp_fusion.s || exit $?
  $ riscv64-linux-gnu-gcc -static cmp_fusion.s ../runtime/runtime.c -o cmp_fusion.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cmp_fusion.bin && echo " exit=0" || echo " exit=$?"
  10305080901101301601801902102401357 exit=0
  $ grep -c 'beqz	t2' cmp_fusion.s
  17

Inside a let-bound branch the inner comparisons fuse in nested
position while twelve values stay live; the outer comparison
materializes its boolean, since its if is bound by let, and is tested
with beq against the false immediate.  The two fused sites plus the
helper's own comparison are the three beqz t2 sites of the file.

  $ cat > cmp_nested.ml <<'EOF'
  > let rec value x = if x = 0 then 0 else 1 + value (x - 1);;
  > let press s =
  >   let a = value 1 in
  >   let b = a + value 2 in
  >   let c = b + value 3 in
  >   let d = c + value 4 in
  >   let e = d + value 5 in
  >   let f = e + value 6 in
  >   let g = f + value 7 in
  >   let h = g + value 8 in
  >   let i = h + value 9 in
  >   let j = i + value 10 in
  >   let k = j + value 11 in
  >   let l = k + value 12 in
  >   let r =
  >     if l < 900
  >     then (if 1 < b then a * 100 + b else b * 100 + a)
  >     else (if 1 > c then c * 3 else a + c)
  >   in
  >   r * 2 + a + b + c + d + e + f + g + h + i + j + k + l;;
  > print_int (press 0)
  > EOF
  $ ../compiler.exe cmp_nested.ml -o cmp_nested.s || exit $?
  $ riscv64-linux-gnu-gcc -static cmp_nested.s ../runtime/runtime.c -o cmp_nested.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cmp_nested.bin && echo " exit=0" || echo " exit=$?"
  570 exit=0
  $ grep -c 'beqz	t2' cmp_nested.s
  3

The comparison result is captured by a closure or stored in a recursive
tuple, so the branch must not fuse and the boolean stays materialized;
only the helper's own comparison fuses, leaving a single beqz t2 site
in the file.

  $ cat > cmp_nofuse.ml <<'EOF'
  > let rec value x = if x = 0 then 0 else 1 + value (x - 1);;
  > let cap t = let c = t < 5 in if c then (fun z -> if c then z else 20) 10 else 30;;
  > print_int (cap (value 3));;
  > print_int (cap (value 9));;
  > let userec t =
  >   let c = t < 5 in
  >   if c then (let rec p = (c, 1) in let (x, y) = p in if x then 14 else 15) else 16;;
  > print_int (userec (value 3));;
  > print_int (userec (value 9))
  > EOF
  $ ../compiler.exe cmp_nofuse.ml -o cmp_nofuse.s || exit $?
  $ riscv64-linux-gnu-gcc -static cmp_nofuse.s ../runtime/runtime.c -o cmp_nofuse.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cmp_nofuse.bin && echo " exit=0" || echo " exit=$?"
  10301416 exit=0
  $ grep -c 'beqz	t2' cmp_nofuse.s
  1

Boundary literals, an underscored literal, and the dynamic negation of
the smallest integer, which wraps like in OCaml.

  $ cat > literals.ml <<'EOF'
  > print_int 4611686018427387903;;
  > print_int (-4611686018427387904);;
  > print_int 1_000_000;;
  > let m = -4611686018427387904 in print_int (- m);;
  > let m = -4611686018427387904 in print_int (~- m)
  > EOF
  $ ../compiler.exe literals.ml -o literals.s || exit $?
  $ riscv64-linux-gnu-gcc -static literals.s ../runtime/runtime.c -o literals.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./literals.bin && echo " exit=0" || echo " exit=$?"
  4611686018427387903-46116860184273879041000000-4611686018427387904-4611686018427387904 exit=0

A function of 300 parameters needs an outgoing argument area larger than
2 KiB; the program is generated by awk to keep this file small.

  $ awk 'BEGIN { printf "let f"; for (i = 1; i <= 300; i++) printf " p%d", i; printf " = p1 + p2 + p300;;\nprint_int (f"; for (i = 1; i <= 300; i++) printf " %d", i; printf ")\n" }' > params300.ml
  $ ../compiler.exe params300.ml -o params300.s || exit $?
  $ riscv64-linux-gnu-gcc -static params300.s ../runtime/runtime.c -o params300.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./params300.bin && echo " exit=0" || echo " exit=$?"
  303 exit=0

Constants around the 12-bit immediate boundary, the 63-bit integer bounds
and wrap-around of addition, division, negation and multiplication.

  $ cat > constants.ml <<'EOF'
  > print_int 4611686018427387903;;
  > print_int (-4611686018427387904);;
  > print_int (4611686018427387903 + 1);;
  > print_int ((-4611686018427387904) / (-1));;
  > print_int ((-4611686018427387904) mod (-1));;
  > print_int 2047;;
  > print_int 2048;;
  > print_int (-2049);;
  > let x = 2000 in print_int (x + 2048);;
  > let y = 2000 in print_int (y - (-2000));;
  > print_int (2047 + 2047);;
  > let z = 4611686018427387903 in print_int (z + z);;
  > print_int ((-4611686018427387904) * 2)
  > EOF
  $ ../compiler.exe constants.ml -o constants.s || exit $?
  $ riscv64-linux-gnu-gcc -static constants.s ../runtime/runtime.c -o constants.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./constants.bin && echo " exit=0" || echo " exit=$?"
  4611686018427387903-4611686018427387904-4611686018427387904-4611686018427387904020472048-2049404840004094-20 exit=0

Arguments and tuple fields are evaluated right to left.

  $ cat > order.ml <<'EOF'
  > let f a b = 0;;
  > print_int (f (print_int 1) (print_int 2));;
  > let t = (print_int 1, print_int 2);;
  > let u = print_int 3;;
  > ()
  > EOF
  $ ../compiler.exe order.ml -o order.s || exit $?
  $ riscv64-linux-gnu-gcc -static order.s ../runtime/runtime.c -o order.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./order.bin && echo " exit=0" || echo " exit=$?"
  210213 exit=0

A shadowed print_int is an ordinary function and prints nothing.

  $ cat > shadow.ml <<'EOF'
  > let print_int x = 0;;
  > print_int 5;;
  > let f a b = a + b in print_int (f 1 2)
  > EOF
  $ ../compiler.exe shadow.ml -o shadow.s || exit $?
  $ riscv64-linux-gnu-gcc -static shadow.s ../runtime/runtime.c -o shadow.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./shadow.bin && echo " exit=0" || echo " exit=$?"
   exit=0

Division by zero, modulus zero and comparing functions are fatal errors
with exit code 2; 2>&1 shows the message of the runtime.

  $ cat > div0.ml <<'EOF'
  > print_int (1 / 0)
  > EOF
  $ ../compiler.exe div0.ml -o div0.s || exit $?
  $ riscv64-linux-gnu-gcc -static div0.s ../runtime/runtime.c -o div0.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./div0.bin 2>&1 && echo " exit=0" || echo " exit=$?"
  Fatal error: exception Division_by_zero
   exit=2

  $ cat > mod0.ml <<'EOF'
  > let x = 5 mod 0 in 0
  > EOF
  $ ../compiler.exe mod0.ml -o mod0.s || exit $?
  $ riscv64-linux-gnu-gcc -static mod0.s ../runtime/runtime.c -o mod0.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./mod0.bin 2>&1 && echo " exit=0" || echo " exit=$?"
  Fatal error: exception Division_by_zero
   exit=2

  $ cat > cmp_eq.ml <<'EOF'
  > print_int (if (1, fun x -> x) = (1, fun x -> x) then 1 else 0)
  > EOF
  $ ../compiler.exe cmp_eq.ml -o cmp_eq.s || exit $?
  $ riscv64-linux-gnu-gcc -static cmp_eq.s ../runtime/runtime.c -o cmp_eq.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cmp_eq.bin 2>&1 && echo " exit=0" || echo " exit=$?"
  Fatal error: exception Invalid_argument("compare: functional value")
   exit=2

Structural comparison short-circuits on the first differing field, before
reaching the closures, and answers true.

  $ cat > cmp_lt.ml <<'EOF'
  > if (1, fun x -> x) < (2, fun x -> x) then print_int 1 else print_int 0
  > EOF
  $ ../compiler.exe cmp_lt.ml -o cmp_lt.s || exit $?
  $ riscv64-linux-gnu-gcc -static cmp_lt.s ../runtime/runtime.c -o cmp_lt.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./cmp_lt.bin && echo " exit=0" || echo " exit=$?"
  1 exit=0

Recursive values: a tuple captures its own address in a back-edge.

  $ cat > rec_tuple.ml <<'EOF'
  > let rec p = (42, fun () -> let (n, _) = p in n) in let (_, f) = p in print_int (f ())
  > EOF
  $ ../compiler.exe rec_tuple.ml -o rec_tuple.s || exit $?
  $ riscv64-linux-gnu-gcc -static rec_tuple.s ../runtime/runtime.c -o rec_tuple.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./rec_tuple.bin && echo " exit=0" || echo " exit=$?"
  42 exit=0

A recursive value aliased by a closure.

  $ cat > rec_fun.ml <<'EOF'
  > let rec f = let g = fun n -> if n = 0 then 42 else f (n - 1) in g in print_int (f 3)
  > EOF
  $ ../compiler.exe rec_fun.ml -o rec_fun.s || exit $?
  $ riscv64-linux-gnu-gcc -static rec_fun.s ../runtime/runtime.c -o rec_fun.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./rec_fun.bin && echo " exit=0" || echo " exit=$?"
  42 exit=0

A recursive scalar computed at run time goes through the rec slot.

  $ cat > rec_arith.ml <<'EOF'
  > let rec x = 40 + 2 in print_int x
  > EOF
  $ ../compiler.exe rec_arith.ml -o rec_arith.s || exit $?
  $ riscv64-linux-gnu-gcc -static rec_arith.s ../runtime/runtime.c -o rec_arith.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./rec_arith.bin && echo " exit=0" || echo " exit=$?"
  42 exit=0

A discarded self-reference is allowed.

  $ cat > rec_discard.ml <<'EOF'
  > let rec x = let _ = x in 42 in print_int x
  > EOF
  $ ../compiler.exe rec_discard.ml -o rec_discard.s || exit $?
  $ riscv64-linux-gnu-gcc -static rec_discard.s ../runtime/runtime.c -o rec_discard.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./rec_discard.bin && echo " exit=0" || echo " exit=$?"
  42 exit=0

A recursive initializer that divides by zero dies at definition time.

  $ cat > rec_div0.ml <<'EOF'
  > let rec x = 1 / 0 in 42
  > EOF
  $ ../compiler.exe rec_div0.ml -o rec_div0.s || exit $?
  $ riscv64-linux-gnu-gcc -static rec_div0.s ../runtime/runtime.c -o rec_div0.bin || exit $?
  $ timeout --kill-after=1s 10 qemu-riscv64 ./rec_div0.bin 2>&1 && echo " exit=0" || echo " exit=$?"
  Fatal error: exception Division_by_zero
   exit=2
