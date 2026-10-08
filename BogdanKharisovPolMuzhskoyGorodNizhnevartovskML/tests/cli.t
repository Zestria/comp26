Standard input uses Angstrom by default.

  $ printf '1\n' | ../compiler.exe
  [Evaluation (Int "1")]

Menhir reads a file containing definitions and evaluations.

  $ cat > input <<'EOF'
  > let x = 1;; x + 2
  > EOF
  $ ../compiler.exe --parser menhir input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

Explicit Angstrom selection and explicit standard input.

  $ ../compiler.exe --parser angstrom - < input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

Empty input is an empty program.

  $ printf '' | ../compiler.exe
  []

An option-like filename can follow --.

  $ cp input ./-input
  $ ../compiler.exe -- -input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

A Menhir parse error goes to stderr with exit code 1.

  $ printf 'let x =' | ../compiler.exe --parser menhir >out 2>err
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

An unknown parser is a command-line error.

  $ ../compiler.exe --parser unknown >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: Unknown parser: unknown; expected angstrom or menhir.

Multiple files are rejected.

  $ ../compiler.exe input input >out 2>err
  [2]
  $ cat out
  $ head -n 1 err
  ../compiler.exe: Expected at most one input file.

Help succeeds without opening the input file.

  $ ../compiler.exe missing-input --help
  Usage: compiler [--parser angstrom|menhir] [--operator-extension] [FILE]
  Without FILE, read standard input.
    --parser NAME Use angstrom (default) or menhir
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

  $ ../compiler.exe --operator-extension --parser menhir missing-input >out 2>err
  [2]
  $ cat out
  $ cat err
  --operator-extension requires the Angstrom parser

Option order does not bypass the parser restriction.

  $ ../compiler.exe --parser menhir --operator-extension missing-input >out 2>err
  [2]
  $ cat out
  $ cat err
  --operator-extension requires the Angstrom parser
