Functions move to top-level definitions, while partial applications stay local.

  $ cat > input.ml <<'ML'
  > let make = fun x -> let y = x + 1 in fun z -> y + z in
  > let f = make 10 in
  > let result = f 2 in
  > print_int result
  > ML
  $ ../compiler.exe --stop-after lifting -o lifted.ml input.ml
  $ cat lifted.ml
  let lifted_fun1 cc_capture0 z = cc_capture0 + z
  
  let lifted_fun0 x =
    let y = x + 1 in
    lifted_fun1 y
  
  let lifted_main0 =
    let make = lifted_fun0 in
    let f = make 10 in
    let result = f 2 in
    print_int result

Both parsers and stdin produce the same lifted program.

  $ ../compiler.exe --stop-after lifting -parsing menhir -o menhir.ml input.ml
  $ cmp lifted.ml menhir.ml
  $ ../compiler.exe --stop-after lifting -o - < input.ml > stdin.ml
  $ cmp lifted.ml stdin.ml

Generated code is executable OCaml and accepted by the source parser.

  $ ocamlc -o lifted.exe lifted.ml
  $ ./lifted.exe; echo
  13
  $ ../compiler.exe --stop-after parsing lifted.ml > /dev/null

The new stage appears in help and error messages.

  $ ../compiler.exe --help | grep '^  --stop-after'
    --stop-after parsing|anf|closure|lifting  Stop after the stage and print its result (default: anf)
  $ ../compiler.exe --stop-after unknown
  Error: --stop-after expects 'parsing', 'anf', 'closure' or 'lifting'
  Try '--help' for usage.
  [2]
