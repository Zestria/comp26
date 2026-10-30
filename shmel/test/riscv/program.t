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

Earlier stages remain available.

  $ ../../compiler.exe --stop-after anf input.ml
  print_int 42
  $ ../../compiler.exe --stop-after unknown
  Error: --stop-after expects 'parsing', 'anf', 'closure', 'lifting' or 'riscv'
  Try '--help' for usage.
  [2]
