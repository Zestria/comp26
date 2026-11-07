Compiling and running the CPS factorial program under QEMU

  $ cat > fact.ml <<- EOF
  > let rec fack n k = if n = 0 then k 1 else fack (n - 1) (fun v -> k (n * v)) in
  > let n = fack 5 (fun x -> x) in
  > print_int n
  > EOF

  $ ../../compiler.exe fact.ml > fact.S
  $ riscv64-linux-gnu-as -march=rv64gc fact.S -o fact.o
Using gcc only for runtime and linking with clib
  $ riscv64-linux-gnu-gcc -c ../runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc -c ../gc.c -o gc.o
  $ riscv64-linux-gnu-gcc -c ../tuple.c -o tuple.o
  $ riscv64-linux-gnu-gcc -c ../closure.c -o closure.o
  $ riscv64-linux-gnu-gcc -nostartfiles fact.o runtime.o gc.o tuple.o closure.o -o fact.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu max ./fact.exe
  120
