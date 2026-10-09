Standard input uses Angstrom by default.

  $ printf '1\n' | ../compiler.exe
  [Evaluation (Int "1")]

ANF output contains normalized computations and evaluates the structure to unit.

  $ printf 'let x = 1 + 2;; x + 3' | ../compiler.exe --stop-after anf -o -
  let x_0 = (+) 1 2 in
  let _ = (+) x_0 3 in
  ()

Menhir also reaches ANF and writes to the requested file.

  $ printf 'let x = 1 + 2;; x + 3' > anf-input
  $ ../compiler.exe -parsing menhir --stop-after anf -o anf-output anf-input
  $ cat anf-output
  let x_0 = (+) 1 2 in
  let _ = (+) x_0 3 in
  ()

An empty program has no computations and returns unit.

  $ printf '' | ../compiler.exe --stop-after anf
  ()

The last requested stage wins; parsing still accepts unresolved names.

  $ printf 'unknown' | ../compiler.exe --stop-after anf --stop-after parsing
  [Evaluation (Var "unknown")]

A lowering error preserves an existing output file and reports to stderr.

  $ printf 'keep me\n' > preserved-anf-output
  $ printf 'unknown' | ../compiler.exe --stop-after anf -o preserved-anf-output >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Unbound value unknown
  $ cat preserved-anf-output
  keep me

A lowering error also does not create a new output file.

  $ printf 'unknown' | ../compiler.exe --stop-after anf -o absent-anf-output >out 2>err
  [1]
  $ test ! -e absent-anf-output

Menhir reads a file containing definitions and evaluations.

  $ cat > input <<'EOF'
  > let x = 1;; x + 2
  > EOF
  $ ../compiler.exe -parsing menhir input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

Explicit Angstrom selection and explicit standard input.

  $ ../compiler.exe -parsing topdown - < input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

Empty input is an empty program.

  $ printf '' | ../compiler.exe
  []

An option-like filename can follow --.

  $ cp input ./-input
  $ ../compiler.exe -- -input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

A Menhir parse error goes to stderr with exit code 1.

  $ printf 'let x =' | ../compiler.exe -parsing menhir >out 2>err
  [1]
  $ cat out
  $ cat err
  Parse error: Syntax error

An Angstrom parse error also leaves stdout empty.

  $ printf 'let x =' | ../compiler.exe >out 2>err
  [1]
  $ cat out
  $ test -s err

A missing file reports an I/O error.

  $ ../compiler.exe missing-input >out 2>err
  [1]
  $ cat out
  $ cat err
  I/O error: missing-input: No such file or directory

Multiple files are rejected.

  $ ../compiler.exe input input >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: Expected at most one input file.

Help succeeds without opening the input file.

  $ ../compiler.exe missing-input --help
  Usage: compiler [-parsing topdown|menhir] [--stop-after parsing|anf] [-o FILE] [--operator-extension] [FILE]
  Without FILE, read standard input.
    -parsing NAME Use topdown (default) or menhir
    --stop-after STAGE Stop after parsing (default) or anf
    -o FILE Write to FILE or - for stdout (default)
    --operator-extension  Enable operator declarations (Angstrom only)
    -  Read standard input
    --  Treat remaining arguments as filenames
    -help  Display this list of options
    --help  Display this list of options

Operator declarations require the extension flag to affect parsing.

  $ printf 'infixr 6 (+);; a + b + c' | ../compiler.exe --operator-extension
  [Evaluation (Apply (Var "+", [Var "a"; Apply (Var "+", [Var "b"; Var "c"])]))]
  $ printf 'infixr 6 (+);; a + b + c' | ../compiler.exe
  [Evaluation (Apply (Var "infixr", [Int "6"; Var "+"])); Evaluation (Apply (Var "+", [Apply (Var "+", [Var "a"; Var "b"]); Var "c"]))]

The extension supports prefix arities beyond two.

  $ printf 'prefix 3 (%%%%);; %%%% a b c' | ../compiler.exe --operator-extension
  [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))]

Menhir rejects the extension option before opening the input file.

  $ ../compiler.exe --operator-extension -parsing menhir missing-input >out 2>err
  [2]
  $ cat out
  $ cat err
  --operator-extension requires the Angstrom parser

Topdown selection overrides a preceding Menhir selection.

  $ printf 'prefix 3 (%%%%);; %%%% a b c' | ../compiler.exe -parsing menhir -parsing topdown --operator-extension
  [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))]

A later Menhir selection overrides a preceding topdown selection.

  $ ../compiler.exe -parsing topdown -parsing menhir --operator-extension missing-input
  --operator-extension requires the Angstrom parser
  [2]

The parsing stage preserves syntactically valid input without type checking.

  $ printf 'if 1 then 2 else false' | ../compiler.exe --stop-after parsing
  [Evaluation (If (Int "1", Int "2", Some (false)))]

Explicit stdout output.

  $ printf '1' | ../compiler.exe -o -
  [Evaluation (Int "1")]

File output replaces existing contents, accepts spaces, and leaves stdout empty.

  $ printf 'old contents that must be replaced\n' > 'result ast'
  $ printf '1' | ../compiler.exe -o 'result ast'
  $ cat 'result ast'
  [Evaluation (Int "1")]

An empty program is still written to the output file.

  $ printf '' | ../compiler.exe --stop-after parsing -o empty-result
  $ cat empty-result
  []

Input is read before an output with the same path is opened.

  $ printf '1' > same-path
  $ ../compiler.exe -o same-path same-path
  $ cat same-path
  [Evaluation (Int "1")]

Only the last output option takes effect.

  $ printf '1' | ../compiler.exe -o unused-result -o -
  [Evaluation (Int "1")]
  $ test ! -e unused-result

An output error goes to stderr with exit code 1.

  $ printf '1' | ../compiler.exe -o missing-directory/result >out 2>err
  [1]
  $ cat out
  $ cat err
  I/O error: missing-directory/result: No such file or directory

A parse error preserves an existing output file.

  $ printf 'keep me\n' > preserved-result
  $ printf 'let x =' | ../compiler.exe -parsing menhir -o preserved-result >out 2>err
  [1]
  $ cat out
  $ cat err
  Parse error: Syntax error
  $ cat preserved-result
  keep me

A parse error does not create a new output file.

  $ printf 'let x =' | ../compiler.exe -parsing menhir -o absent-result >out 2>err
  [1]
  $ test ! -e absent-result

An unknown parser is rejected before opening the input file.

  $ ../compiler.exe -parsing unknown missing-input >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: Unknown parser: unknown; expected topdown or menhir.

An unknown stage fails before opening input or output.

  $ ../compiler.exe --stop-after unknown -o absent-stage-result missing-input >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: Unknown stage: unknown; expected parsing or anf.
  $ test ! -e absent-stage-result

The output option requires a filename.

  $ ../compiler.exe -o >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: option '-o' needs an argument.

The parser option requires a parser name.

  $ ../compiler.exe -parsing >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: option '-parsing' needs an argument.

The stage option requires a stage name.

  $ ../compiler.exe --stop-after >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: option '--stop-after' needs an argument.

Option order does not bypass the parser restriction.

  $ ../compiler.exe -parsing menhir --operator-extension missing-input >out 2>err
  [2]
  $ cat out
  $ cat err
  --operator-extension requires the Angstrom parser
