Compiling and running the CPS fibonacci program under QEMU

  $ cat > fib.ml <<- EOF
  > let rec fib n k =
  >   if n < 2 then k n else fib (n - 1) (fun x -> fib (n - 2) (fun y -> k (x + y)))
  > in
  > let n = fib 6 (fun x -> x) in
  > print_int n
  > EOF

  $ ../../compiler.exe fib.ml > fib.S
  $ riscv64-linux-gnu-as -march=rv64gc fib.S -o fib.o
Using gcc only for runtime and linking with clib
  $ riscv64-linux-gnu-gcc -c ../runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc -c ../gc.c -o gc.o
  $ riscv64-linux-gnu-gcc -c ../tuple.c -o tuple.o
  $ riscv64-linux-gnu-gcc -c ../closure.c -o closure.o
  $ riscv64-linux-gnu-gcc -nostartfiles fib.o runtime.o gc.o tuple.o closure.o -o fib.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64,c=true ./fib.exe
  8
