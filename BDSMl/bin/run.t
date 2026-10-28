
  $ ../compiler.exe --parse '8 - 3 - 1'
  BinOp ('-', BinOp ('-', Int 8, Int 3), Int 1)
  $ ../compiler.exe --parse '8 - 3 - 1' --operator-assoc minus:right
  BinOp ('-', Int 8, BinOp ('-', Int 3, Int 1))
  $ ../compiler.exe --parse 'true || false || true'
  BinOp ('||', Bool true, BinOp ('||', Bool false, Bool true))
  $ ../compiler.exe --parse 'true || false || true' --operator-assoc or:left
  BinOp ('||', BinOp ('||', Bool true, Bool false), Bool true)

  $ ../compiler.exe -parsing menhir --stop-after parsing cli_input.ml -o -
  let main = (print_int (1 + 2));;
  $ ../compiler.exe -parsing topdown --stop-after parsing cli_input.ml -o parsed.ml
  $ cat parsed.ml
  let main = (print_int (1 + 2));;
  $ ../compiler.exe -parsing topdown --stop-after cc cli_input.ml -o -
  let main_1 = (print_int (1 + 2));;
  $ ../compiler.exe -parsing topdown --stop-after cc closure_input.ml -o closure_output.ml
  $ ocaml closure_output.ml
  7

  $ ../compiler.exe -parsing topdown --stop-after ll fac_cps.ml -o fact_lifted.ml
  $ ocaml fact_lifted.ml
  120
  $ ../compiler.exe -parsing topdown --stop-after anf fac_cps.ml -o fact_anf.ml
  $ ocaml fact_anf.ml
  120
  $ ../compiler.exe -parsing menhir --stop-after anf fib_cps.ml -o fib_anf.ml
  $ ocaml fib_anf.ml
  55
  $ ../compiler.exe -parsing menhir --stop-after anf recursive_capture.ml -o recursive_capture_anf.ml
  $ ocaml recursive_capture_anf.ml
  0

  $ sh check_stages.sh
  All stages preserve the output of all examples.

  $ echo 'let main = let rec x = x in print_int x;;' > invalid_anf.ml
  $ ../compiler.exe --stop-after anf invalid_anf.ml
  Recursive initializer for x_2 is not supported
  [2]

  $ ../compiler.exe --parser=lr --parse='1 + 2'
  BinOp ('+', Int 1, Int 2)
  $ ../compiler.exe -parsing menhir --stop-after parsing -- cli_input.ml
  let main = (print_int (1 + 2));;
  $ ../compiler.exe --parser unknown 2> /dev/null
  [2]
  $ ../compiler.exe --stop-after 2> /dev/null
  [2]
  $ ../compiler.exe --stop-after unknown cli_input.ml 2> /dev/null
  [2]
  $ ../compiler.exe cli_input.ml closure_input.ml 2> /dev/null
  [2]
  $ ../compiler.exe --parse 1 cli_input.ml 2> /dev/null
  [2]
  $ ../compiler.exe --parser lr --operator-assoc minus:right --parse '8 - 3 - 1' 2> /dev/null
  [2]
