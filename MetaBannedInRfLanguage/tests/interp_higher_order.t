AST and ANF interpreters agree on a higher-order function

  $ cat > higher_order.ml <<EOF
  > let apply_twice f x = f (f x);;
  > let inc = fun x -> x + 1;;
  > let r = apply_twice inc 3
  > EOF

  $ ./interp_check.exe higher_order.ml
  apply_twice = <closure>
  inc = <closure>
  r = 5

  $ ./interp_check.exe --full higher_order.ml
  apply_twice = <closure>
  inc = <closure>
  r = 5
