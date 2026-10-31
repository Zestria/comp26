The parsing stage uses Angstrom by default.

  $ printf '1\n' | ../compiler.exe --stop-after parsing
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

ANF eliminates known tuple projections and unreachable branches before printing.

  $ printf 'let (a, b) = (40, 2);; if true then a + b else 1 / 0' | ../compiler.exe --stop-after anf
  let _ = (+) 40 2 in
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
  $ ../compiler.exe --stop-after parsing -parsing menhir input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

Explicit Angstrom selection and explicit standard input.

  $ ../compiler.exe --stop-after parsing -parsing topdown - < input
  [Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]

Empty input is an empty program.

  $ printf '' | ../compiler.exe --stop-after parsing
  []

An option-like filename can follow --.

  $ cp input ./-input
  $ ../compiler.exe --stop-after parsing -- -input
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
  Usage: compiler [-parsing topdown|menhir] [--stop-after parsing|anf|cc|ll|riscv] [-o FILE] [--operator-extension] [FILE]
  Without FILE, read standard input.
    -parsing NAME Use topdown (default) or menhir
    --stop-after STAGE Stop after parsing, anf, cc, ll or riscv (default: riscv)
    -o FILE Write to FILE or - for stdout (default)
    --operator-extension  Enable operator declarations (Angstrom only)
    -  Read standard input
    --  Treat remaining arguments as filenames
    -help  Display this list of options
    --help  Display this list of options

Operator declarations require the extension flag to affect parsing.

  $ printf 'infixr 6 (+);; a + b + c' | ../compiler.exe --stop-after parsing --operator-extension
  [Evaluation (Apply (Var "+", [Var "a"; Apply (Var "+", [Var "b"; Var "c"])]))]
  $ printf 'infixr 6 (+);; a + b + c' | ../compiler.exe --stop-after parsing
  [Evaluation (Apply (Var "infixr", [Int "6"; Var "+"])); Evaluation (Apply (Var "+", [Apply (Var "+", [Var "a"; Var "b"]); Var "c"]))]

The extension supports prefix arities beyond two.

  $ printf 'prefix 3 (%%%%);; %%%% a b c' | ../compiler.exe --stop-after parsing --operator-extension
  [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))]

Menhir rejects the extension option before opening the input file.

  $ ../compiler.exe --operator-extension -parsing menhir missing-input >out 2>err
  [2]
  $ cat out
  $ cat err
  --operator-extension requires the Angstrom parser

Topdown selection overrides a preceding Menhir selection.

  $ printf 'prefix 3 (%%%%);; %%%% a b c' | ../compiler.exe --stop-after parsing -parsing menhir -parsing topdown --operator-extension
  [Evaluation (Apply (Var "%%", [Var "a"; Var "b"; Var "c"]))]

A later Menhir selection overrides a preceding topdown selection.

  $ ../compiler.exe -parsing topdown -parsing menhir --operator-extension missing-input
  --operator-extension requires the Angstrom parser
  [2]

The parsing stage preserves syntactically valid input without type checking.

  $ printf 'if 1 then 2 else false' | ../compiler.exe --stop-after parsing
  [Evaluation (If (Int "1", Int "2", Some (false)))]

Explicit stdout output.

  $ printf '1' | ../compiler.exe --stop-after parsing -o -
  [Evaluation (Int "1")]

File output replaces existing contents, accepts spaces, and leaves stdout empty.

  $ printf 'old contents that must be replaced\n' > 'result ast'
  $ printf '1' | ../compiler.exe --stop-after parsing -o 'result ast'
  $ cat 'result ast'
  [Evaluation (Int "1")]

An empty program is still written to the output file.

  $ printf '' | ../compiler.exe --stop-after parsing -o empty-result
  $ cat empty-result
  []

Input is read before an output with the same path is opened.

  $ printf '1' > same-path
  $ ../compiler.exe --stop-after parsing -o same-path same-path
  $ cat same-path
  [Evaluation (Int "1")]

Only the last output option takes effect.

  $ printf '1' | ../compiler.exe --stop-after parsing -o unused-result -o -
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
  ../compiler.exe: Unknown stage: unknown; expected parsing, anf, cc, ll or riscv.
  $ test ! -e absent-stage-result

An unknown stage preserves an existing output file.

  $ printf 'keep me\n' > preserved-stage-result
  $ ../compiler.exe --stop-after unknown -o preserved-stage-result missing-input >out 2>err
  [2]
  $ cat preserved-stage-result
  keep me

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

The LL stage lifts nested code and makes closure captures explicit.

  $ printf 'let make x = fun y -> x + y;; make 40 2' > closure-input
  $ ../compiler.exe --stop-after ll closure-input | sed '/^$/d'
  code code_0 [x_1] y_2 =
    (+) x_1 y_2
  code code_3 [] x_4 =
    closure code_0 [x_4]
  let make_5 = closure code_3 [] in
  let _ = make_5 40 2 in
  ()

Menhir also runs the full pipeline and writes lifted code to a file.

  $ ../compiler.exe -parsing menhir --stop-after ll closure-input -o closure-output
  $ sed '/^$/d' closure-output
  code code_0 [x_1] y_2 =
    (+) x_1 y_2
  code code_3 [] x_4 =
    closure code_0 [x_4]
  let make_5 = closure code_3 [] in
  let _ = make_5 40 2 in
  ()

Stopping after ANF retains nested functions and does not perform lifting.

  $ ../compiler.exe closure-input --stop-after anf
  let make_0 = fun x_1 -> fun y_2 -> (+) x_1 y_2 in
  let _ = make_0 40 2 in
  ()

Stopping after CC shows construction captures separately from fresh function slots.

  $ ../compiler.exe -parsing topdown closure-input --stop-after cc -o -
  let make_0 = closure [] (fun [] x_1 ->
                 closure [x_1] (fun [x_2] y_3 -> (+) x_2 y_3)) in
  let _ = make_0 40 2 in
  ()

Menhir also stops after CC and writes the nested code to a file.

  $ ../compiler.exe -parsing menhir closure-input --stop-after cc -o cc-output
  $ cat cc-output
  let make_0 = closure [] (fun [] x_1 ->
                 closure [x_1] (fun [x_2] y_3 -> (+) x_2 y_3)) in
  let _ = make_0 40 2 in
  ()

Explicit LL with topdown and stdout is identical to LL file output.

  $ ../compiler.exe -parsing topdown closure-input --stop-after ll -o - > explicit-ll-output
  $ cmp closure-output explicit-ll-output

Explicit LL with Menhir and file output is also identical.

  $ ../compiler.exe -parsing menhir closure-input --stop-after ll -o explicit-ll-file
  $ cmp closure-output explicit-ll-file

A final CC option overrides an earlier LL option.

  $ ../compiler.exe closure-input --stop-after ll --stop-after cc -o last-cc-output
  $ cmp cc-output last-cc-output

A final LL option overrides an earlier CC option.

  $ ../compiler.exe closure-input --stop-after cc --stop-after ll -o last-ll-output
  $ cmp closure-output last-ll-output

Parsing can still override CC and omit semantic checks.

  $ printf 'unknown' | ../compiler.exe --stop-after cc --stop-after parsing
  [Evaluation (Var "unknown")]

CC rejects unresolved names before creating an output file.

  $ printf 'unknown' | ../compiler.exe --stop-after cc -o absent-cc-output >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Unbound value unknown
  $ test ! -e absent-cc-output

CC checks recursive source patterns before normalization and preserves existing output.

  $ printf 'keep me\n' > preserved-cc-output
  $ printf 'let rec p = let (_, _) = p in (1, 2)' | ../compiler.exe -parsing menhir --stop-after cc -o preserved-cc-output >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: This kind of expression is not allowed on the right of let rec p
  $ cat preserved-cc-output
  keep me

An empty program can stop after CC.

  $ printf '' | ../compiler.exe --stop-after cc
  ()

An empty program also passes through the LL pipeline.

  $ printf '' | ../compiler.exe --stop-after ll
  ()

A recursive tuple captures its own address without inspecting it during construction.

  $ printf 'let rec p = (42, fun () -> let (n, _) = p in n)' | ../compiler.exe --stop-after ll | sed '/^$/d'
  code code_0 [p_1] arg_2 =
    field[0] p_1
  let rec p_3 = (let tmp_4 = closure code_0 [p_3] in
                 (42, tmp_4)) in
  ()

An unsafe recursive initializer is rejected before replacing the output.

  $ printf 'keep me\n' > preserved-rec-output
  $ printf 'let rec x = x + 1' | ../compiler.exe --stop-after ll -o preserved-rec-output >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: This kind of expression is not allowed on the right of let rec x
  $ cat preserved-rec-output
  keep me

An unsafe initializer also does not create an absent output file.

  $ printf 'let rec x = x' | ../compiler.exe -parsing menhir --stop-after ll -o absent-rec-output >out 2>err
  [1]
  $ test ! -e absent-rec-output

The operator extension also reaches the full middleend with its declared arity.

  $ printf 'prefix 3 (%%%%);; let (%%%%) a b c = a + b + c;; %%%% 1 2 3' | ../compiler.exe --stop-after ll --operator-extension | sed '/^$/d'
  code code_0 [] a_1 b_2 c_3 =
    let tmp_4 = (+) a_1 b_2 in
    (+) tmp_4 c_3
  let _ = direct code_0 1 2 3 in
  ()

RISC-V assembly is emitted by default and with an explicit stage.

  $ printf 'print_int 42' | ../compiler.exe --stop-after riscv > explicit-riscv-output
  $ printf 'print_int 42' | ../compiler.exe > default-riscv-output
  $ cmp explicit-riscv-output default-riscv-output
  $ grep -c '^ml_main:' explicit-riscv-output
  1

An out-of-range integer is rejected during lowering and preserves an existing output file.

  $ printf 'keep me\n' > preserved-riscv-output
  $ printf 'print_int 4611686018427387904' | ../compiler.exe -o preserved-riscv-output >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Integer literal exceeds the range of representable integers of type int: 4611686018427387904
  $ cat preserved-riscv-output
  keep me

A lowering error also does not create a new output file.

  $ printf 'print_int 4611686018427387904' | ../compiler.exe -o absent-riscv-output >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Integer literal exceeds the range of representable integers of type int: 4611686018427387904
  $ test ! -e absent-riscv-output

An unused out-of-range integer is rejected before it can be optimized away.

  $ printf 'let _ = 4611686018427387904 in print_int 42' | ../compiler.exe >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Integer literal exceeds the range of representable integers of type int: 4611686018427387904

Menhir accepts the boundary literals and an underscored literal.

  $ printf '4611686018427387903' | ../compiler.exe -parsing menhir --stop-after parsing
  [Evaluation (Int "4611686018427387903")]
  $ printf '(-4611686018427387904)' | ../compiler.exe -parsing menhir --stop-after parsing
  [Evaluation (Int "-4611686018427387904")]
  $ printf '1_000_000' | ../compiler.exe -parsing menhir --stop-after parsing
  [Evaluation (Int "1_000_000")]

An out-of-range negative literal is rejected even inside a dead branch.

  $ printf 'if false then print_int (-4611686018427387905) else print_int 1' | ../compiler.exe -parsing menhir >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Integer literal exceeds the range of representable integers of type int: -4611686018427387905

An out-of-range literal is rejected even inside a function that is never called.

  $ printf 'let f x = 4611686018427387904;; print_int 1' | ../compiler.exe >out 2>err
  [1]
  $ cat out
  $ cat err
  Middleend error: Integer literal exceeds the range of representable integers of type int: 4611686018427387904

