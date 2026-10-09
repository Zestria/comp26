AST and ANF interpreters agree on tuple destructuring

  $ cat > tuples.ml <<EOF
  > let (a, b) = (3, 4);;
  > let sum = a + b
  > EOF

  $ ./interp_check.exe tuples.ml
  b = 4
  a = 3
  sum = 7

  $ ./interp_check.exe --full tuples.ml
  b = 4
  a = 3
  sum = 7
