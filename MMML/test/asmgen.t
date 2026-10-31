The backend pipeline (parse, lambda lifting, closure conversion, ANF, RISC-V
assembly) is wired. Output runs on Linux user-mode qemu; print_int uses the
write syscall (a7 = 64).

  $ cat > arith.ml <<'EOF'
  > let _ = print_int (1 + 2 * 3)
  > EOF
  $ ../asmgen.exe arith.ml > arith.s
  $ riscv64-linux-gnu-gcc -static -I ../runtime arith.s ../runtime/runtime.c ../runtime/gc.c ../runtime/startup.c -o arith
  $ qemu-riscv64 ./arith
  7

A bare expression is accepted as well.

  $ ../asmgen.exe <<'EOF' > five.s
  > print_int 5
  > EOF
  $ riscv64-linux-gnu-gcc -static -I ../runtime five.s ../runtime/runtime.c ../runtime/gc.c ../runtime/startup.c -o five
  $ qemu-riscv64 ./five
  5

CPS factorial.

  $ cat > fac.ml <<'EOF'
  > let rec fac n k = if n < 2 then k 1 else fac (n - 1) (fun a -> k (a * n))
  > let _ = fac 5 (fun r -> print_int r)
  > EOF
  $ ../asmgen.exe fac.ml > fac.s
  $ riscv64-linux-gnu-gcc -static -I ../runtime fac.s ../runtime/runtime.c ../runtime/gc.c ../runtime/startup.c -o fac
  $ qemu-riscv64 ./fac
  120

CPS fibonacci.

  $ cat > fib.ml <<'EOF'
  > let rec fib n k = if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
  > let _ = fib 10 (fun r -> print_int r)
  > EOF
  $ ../asmgen.exe fib.ml > fib.s
  $ riscv64-linux-gnu-gcc -static -I ../runtime fib.s ../runtime/runtime.c ../runtime/gc.c ../runtime/startup.c -o fib
  $ qemu-riscv64 ./fib
  55
