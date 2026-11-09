Glued keywords require the explicit flag and the top-down parser.

  $ echo 'letrecf n=ifn<1thennelsef(n-1)' > glued.ml
  $ ../compiler.exe --glued-keywords glued.ml
  Parsed 1 top-level bindings
  $ ../compiler.exe glued.ml 2>/dev/null
  [1]
  $ ../compiler.exe -parsing menhir glued.ml 2>/dev/null
  [1]
  $ ../compiler.exe -parsing menhir --glued-keywords glued.ml
  --glued-keywords requires -parsing topdown
  [2]

Both parsers still accept ordinary source without the flag.

  $ echo 'let x=1' > normal.ml
  $ ../compiler.exe normal.ml
  Parsed 1 top-level bindings
  $ ../compiler.exe -parsing menhir normal.ml
  Parsed 1 top-level bindings
