AST and ANF interpreters agree on a factorial

  $ cat > factorial.ml <<EOF
  > let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;
  > let r = fact 5
  > EOF

  $ ./interp_check.exe factorial.ml
  fact = <closure>
  r = 120
