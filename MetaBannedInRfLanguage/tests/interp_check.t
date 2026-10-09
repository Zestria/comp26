AST and ANF interpreters agree on a factorial

  $ cat > factorial.ml <<EOF
  > let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;
  > let r = fact 5
  > EOF

  $ ./interp_check.exe factorial.ml
  fact = <closure>
  r = 120

  $ ./interp_check.exe --full factorial.ml
  fact = <closure>
  r = 120

AST and ANF interpreters agree on a fibonacci

  $ cat > fibonacci.ml <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);;
  > let r = fib 8
  > EOF

  $ ./interp_check.exe fibonacci.ml
  fib = <closure>
  r = 21

  $ ./interp_check.exe --full fibonacci.ml
  fib = <closure>
  r = 21

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

AST and ANF interpreters agree on pattern matching

  $ cat > match_example.ml <<EOF
  > let classify n = match n with
  >   | 0 -> false
  >   | 1 -> true
  >   | _ -> n > 1
  > ;;
  > let r1 = classify 0;;
  > let r2 = classify 5
  > EOF

  $ ./interp_check.exe match_example.ml
  classify = <closure>
  r1 = false
  r2 = true

  $ ./interp_check.exe --full match_example.ml
  classify = <closure>
  r1 = false
  r2 = true

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
