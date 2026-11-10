Help does not require an input file, even if other options would conflict.

  $ ../compiler.exe --help > help.txt
  $ cat help.txt
  Usage: compiler [-parsing topdown|menhir] [--glued-keywords] input.ml
    -parsing topdown|menhir  Select parser (default: topdown)
    --glued-keywords        Allow keywords inside words (topdown only)
    -h, --help           Show this help
  $ ../compiler.exe -h > short-help.txt
  $ cmp help.txt short-help.txt
  $ ../compiler.exe -parsing menhir --glued-keywords --help > conflict-help.txt
  $ cmp help.txt conflict-help.txt

Options work before or after the input; the last parser selection wins.

  $ echo 'let x=1' > input.ml
  $ ../compiler.exe -parsing topdown input.ml
  Parsed 1 top-level bindings
  $ ../compiler.exe input.ml -parsing menhir
  Parsed 1 top-level bindings
  $ echo 'letx=1' > glued.ml
  $ ../compiler.exe -parsing menhir glued.ml --glued-keywords -parsing topdown
  Parsed 1 top-level bindings

Argument errors exit with status 2.

  $ ../compiler.exe --glued-keywords
  missing input file
  [2]
  $ ../compiler.exe -parsing
  missing value for -parsing
  [2]
  $ ../compiler.exe -parsing other input.ml
  unknown parser: other (expected topdown or menhir)
  [2]
  $ ../compiler.exe --unknown
  unknown option: --unknown
  [2]
  $ ../compiler.exe input.ml input.ml
  expected one input file
  [2]

After --, arguments are filenames rather than options.

  $ echo 'let x=1' > --help
  $ ../compiler.exe -- --help
  Parsed 1 top-level bindings
  $ ../compiler.exe --
  missing input file
  [2]
