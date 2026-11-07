Command-line stages and diagnostics.

  $ printf 'let x = 5 in x + 1' | ../compiler.exe --stop-after anf
  let x = 5 in (x + 1);;
  

  $ printf 'let x = 5 in x + 1' | ../compiler.exe --stop-after lambda-lifting
  let x = 5 in (x + 1);;
  

  $ ../compiler.exe --bogus
  compiler: unrecognized option: --bogus
  Try 'compiler.exe --help' for more information.
  [2]

  $ ../compiler.exe -parsing foo
  compiler: unknown parser: foo (expected 'topdown' or 'menhir')
  Try 'compiler.exe --help' for more information.
  [2]

  $ ../compiler.exe --stop-after bogus
  compiler: unknown stage: bogus (expected 'parsing', 'anf', 'closure-conversion' 'lambda-lifting' or 'riscv')
  Try 'compiler.exe --help' for more information.
  [2]

  $ printf 'print_int 42' | ../compiler.exe -o generated.s
  $ grep -c '^main:' generated.s
  1
  $ printf 'print_int 42' | ../compiler.exe --backend riscv -o - > explicit.s
  $ cmp generated.s explicit.s

  $ printf 'print_int unknown' | ../compiler.exe
  compiler: backend: unbound variable 'unknown'
  [1]
