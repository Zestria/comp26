AST and ANF interpreters agree on a fibonacci

  $ cat > fibonacci.ml <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);;
  > let r = fib 8
  > EOF

  $ ./interp_check.exe fibonacci.ml
  fib = <closure>
  r = 21
