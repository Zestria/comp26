# BogdanKharisovPolMuzhskoyGorodNizhnevartovskML

Compiler for a subset of OCaml.

Implemented stages: parsing, ANF, closure conversion, lambda lifting and
RISC-V 64 code generation.

Two parsers produce the same AST:

- Angstrom: top-down parser.
- Menhir + ocamllex: bottom-up parser.

## Build and test

```console
comp26 $ opam install ./BogdanKharisovPolMuzhskoyGorodNizhnevartovskML.opam --deps-only --with-test
comp26 $ dune build -p BogdanKharisovPolMuzhskoyGorodNizhnevartovskML
comp26 $ dune test -p BogdanKharisovPolMuzhskoyGorodNizhnevartovskML
comp26 $ dune build -p BogdanKharisovPolMuzhskoyGorodNizhnevartovskML @BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/check
comp26 $ zanuda -dir BogdanKharisovPolMuzhskoyGorodNizhnevartovskML
```

## Run

```console
comp26 $ printf 'let x = 1;; x + 2\n' | dune exec BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/compiler.exe -- --stop-after parsing
[Definition (PVar "x" = Int "1"); Evaluation (Apply (Var "+", [Var "x"; Int "2"]))]
```

- `-parsing topdown|menhir`: select the parser; defaults to topdown (Angstrom).
- `--stop-after parsing|anf|cc|ll|riscv`: stop at the source AST, ANF, closure
  conversion, lambda lifting or RISC-V 64 assembly. The default is `riscv`.
- `-o FILE`: write the selected representation to a file; `-o -` writes to stdout (the default).
- `--operator-extension`: enable [operator declarations](#operator-declarations) (Angstrom only).
- `FILE`: input file; omit it or use `-` to read stdin.

When parser, stage or output options are repeated, the last value takes effect.
Input is read and transformed before the output file is opened. A parse or
middleend error leaves an existing output file unchanged. Diagnostics are written
to stderr. Selecting `parsing` does not perform semantic checks.

```console
comp26 $ dune exec BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/compiler.exe -- -parsing menhir --stop-after ll -o program.ll program.ml
comp26 $ dune exec BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/compiler.exe -- --stop-after anf -o program.anf program.ml
comp26 $ dune exec BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/compiler.exe -- --stop-after cc -o program.cc program.ml
```

## RISC-V 64

The default output is RV64IM assembly exporting `ml_main`. Link it with the
runtime and run it under QEMU:

```console
comp26 $ dune exec BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/compiler.exe -- -o program.s program.ml
comp26 $ riscv64-linux-gnu-gcc -static program.s BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/runtime/runtime.c -o program
comp26 $ qemu-riscv64 ./program
```

`print_int` prints an integer without a newline; it is an ordinary builtin
and can be shadowed. Integers are 63-bit and wrap like OCaml's `int`; a literal
outside `[-2^62, 2^62 - 1]` is a compile error. Division by zero and comparing
functions stop the program with `Fatal error: exception ...` and exit code 2.
Programs are assumed to be well typed; there is no type checker yet.
Memory is allocated with a checked `malloc` and never freed (there is no
collector); allocation failure exits with code 2. Tail calls are not optimized
yet, so deep recursion is bounded by the stack.

The runtime is the single C file `runtime/runtime.c`. It contains the program
entry `main` (which calls `ml_main`), allocation, structural comparison,
`print_int`, and generic application with partial and over-application; an
inline assembly bridge `rt_invoke` calls code with any number of arguments.

## Syntax

- Decimal integers with optional underscores, `true`, `false` and `()`.
- ASCII identifiers: `[a-z_][a-zA-Z0-9_']*`, excluding keywords and the wildcard `_`.
- Arithmetic: `+`, `-`, `*`, `/`, `mod` and unary minus.
- Comparisons: `=`, `<>`, `<`, `<=`, `>` and `>=`; logical operators `&&` and `||`.
- Functions with multiple parameters, partial application and functions as values.
  `not x` is an ordinary function application.
- Top-level `let`, local `let ... in` and `let rec`. Recursive groups with
  `and` are not supported.
- `if ... then ...` with an optional `else`.
- Tuples.
- Patterns in bindings and parameters: names, `_`, `()` and tuples.
- Custom symbolic operators and operator names in parentheses, such as `( + )`
  and `( mod )`. Operators can be bound with `let`.
- Nested `(* ... *)` comments. Quoted-string
  delimiters inside comments use lowercase ASCII letters and underscores.

Definitions can follow one another without `;;`. An expression following
another structure item needs `;;`; the first item can be an expression.

Keywords: `else`, `false`, `fun`, `if`, `in`, `let`, `rec`, `then`, `true`, `mod`.
Other OCaml keywords, such as `class`, remain valid identifiers.

Type annotations, string and character literals, lists, `match` expressions,
modules, and labelled or optional arguments are outside this subset.
Integer ranges and recursive-value restrictions are not checked during parsing.

```ocaml
let rec factorial n k =
  if n = 0 then k 1
  else factorial (n - 1) (fun r -> k (n * r))

let rec fibonacci n k =
  if n < 2 then k n
  else fibonacci (n - 1) (fun a ->
    fibonacci (n - 2) (fun b -> k (a + b)))

;; factorial 10 (fun x -> x)
;; fibonacci 10 (fun x -> x)
```

## Operator declarations

| Declaration | Meaning |
| --- | --- |
| `infixl P (OP);;` | Binary infix operator, left associative. |
| `infixr P (OP);;` | Binary infix operator, right associative. |
| `prefix N (OP);;` | Prefix operator taking `N` operands. |

`P` ranges from `0` to `9`; larger values bind more tightly. Ordinary addition
has precedence `6`, multiplication `7`, and power operators `8`.
`N` is a positive integer.

Declarations occur at the top level, end with `;;`, and affect subsequent
input. A later declaration for the same operator replaces the earlier one.
Bind operator values separately with `let`.

```ocaml
infixr 6 (+);;
a + b + c
```

This groups as `a + (b + c)`. Operators at the same precedence with different
associativities require parentheses.

```ocaml
prefix 3 (%%);;
let (%%) a b c = a + b + c;;
%% 1 2 3
```

Parenthesize compound prefix operands: `(f x)`, `(a + b)`, `(-1)`.
`(%%) 1` remains an ordinary partial application.

Application and prefix operators bind more tightly than declared infix
operators. Tuple commas bind between precedence `0` and `1`.

Without the flag, `infixl`, `infixr` and `prefix` are ordinary identifiers.

## Parser benchmark

```console
comp26 $ dune exec BogdanKharisovPolMuzhskoyGorodNizhnevartovskML/bench/benchmark.exe > _build/parser-benchmark.csv
```
