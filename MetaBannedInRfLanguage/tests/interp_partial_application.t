AST and ANF interpreters agree on partial application

  $ cat > partial_application.ml <<EOF
  > let make_adder x = fun y -> x + y;;
  > let add5 = make_adder 5;;
  > let r = add5 3
  > EOF

  $ ./interp_check.exe partial_application.ml
  make_adder = <closure>
  add5 = <closure>
  r = 8

Same result after closure conversion and lambda lifting

  $ ./interp_check.exe --full partial_application.ml
  make_adder = <closure>
  add5 = <closure>
  r = 8
