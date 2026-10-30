The default stage emits the same assembly as the explicit RISC-V stage.

  $ echo 'print_int 42' > input.ml
  $ ../../compiler.exe input.ml > default.s
  $ ../../compiler.exe --stop-after riscv -o explicit.s input.ml
  $ cmp default.s explicit.s
  $ ../../compiler.exe -parsing menhir -o menhir.s input.ml
  $ cmp default.s menhir.s
  $ ../../compiler.exe --stop-after riscv -o - < input.ml > stdin.s
  $ cmp default.s stdin.s
  $ grep '^main:' default.s
  main:
  $ grep 'call shmel_apply' default.s
    call shmel_apply

An error must not overwrite an existing output file.

  $ echo 'keep this file' > output.s
  $ echo 'missing 1' > bad.ml
  $ ../../compiler.exe -o output.s bad.ml
  Backend error: unbound variable missing
  [1]
  $ cat output.s
  keep this file
  $ echo 'let x =' > bad.ml
  $ ../../compiler.exe -o output.s bad.ml > /dev/null 2>&1
  [1]
  $ cat output.s
  keep this file
  $ ../../compiler.exe -o missing/output.s input.ml > /dev/null 2>&1
  [1]

Code arities above eight, including explicit captures, compile with both parsers.

  $ echo 'let f a b c d e f g h i = a + i in print_int (f 1 2 3 4 5 6 7 8 9)' > wide.ml
  $ ../../compiler.exe -o wide.s wide.ml
  $ ../../compiler.exe -parsing menhir -o wide-menhir.s wide.ml
  $ cmp wide.s wide-menhir.s
  $ echo 'let bump x = x + 1 in let f a b c d e f g h = bump a + b + c + d + e + f + g + h in print_int (f 1 2 3 4 5 6 7 8)' > captures.ml
  $ ../../compiler.exe -o captures.s captures.ml
  $ ../../compiler.exe -parsing menhir -o captures-menhir.s captures.ml
  $ cmp captures.s captures-menhir.s
  $ cat output.s
  keep this file

Earlier stages remain available.

  $ ../../compiler.exe --stop-after anf input.ml
  print_int 42
  $ ../../compiler.exe --stop-after unknown
  Error: --stop-after expects 'parsing', 'anf', 'closure', 'lifting' or 'riscv'
  Try '--help' for usage.
  [2]
