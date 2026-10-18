Helpers run the actual executable and check both exit status and streams.

  $ compiler() { ../compiler.exe "$@"; }
  $ usage_error() { compiler "$@" > actual.out 2> actual.err; status=$?; test "$status" -eq 2 && test ! -s actual.out && test -s actual.err; }
  $ runtime_error() { compiler "$@" > actual.out 2> actual.err; status=$?; test "$status" -eq 1 && test ! -s actual.out && test -s actual.err; }

Small outputs have independent expected values, rather than just comparing
two invocations of the same implementation.

  $ compiler --stop-after parsing -parsing topdown fixtures/small.ml
  [(Definition (Nonrecursive, (PVariable "answer"),
      (Application ((Application ((Variable "+"), (Constant (Integer 1)))),
         (Constant (Integer 2))))
      ))
    ]
  $ compiler --stop-after anf -parsing menhir fixtures/small.ml
  let answer#0 =
    1 + 2

Both parser/stage combinations, file/stdin input, argument placement,
defaults, legacy modes, and deterministic CPS output agree.

  $ for source in small factorial fibonacci; do
  >   for stage in parsing anf; do
  >     compiler --stop-after "$stage" -parsing menhir "fixtures/$source.ml" > baseline 2> actual.err || exit 1
  >     test ! -s actual.err || exit 1
  >     test -s baseline || exit 1
  >     compiler "fixtures/$source.ml" -parsing topdown --stop-after "$stage" > actual.out 2> actual.err || exit 1
  >     test ! -s actual.err && cmp baseline actual.out || exit 1
  >     compiler --stop-after "$stage" -parsing topdown < "fixtures/$source.ml" > actual.out || exit 1
  >     cmp baseline actual.out || exit 1
  >     compiler --stop-after "$stage" "fixtures/$source.ml" > actual.out || exit 1
  >     cmp baseline actual.out || exit 1
  >   done
  >   compiler "fixtures/$source.ml" > actual.out || exit 1
  >   cmp baseline actual.out || exit 1
  >   compiler -parsing topdown < "fixtures/$source.ml" > actual.out || exit 1
  >   cmp baseline actual.out || exit 1
  > done
  $ compiler --stop-after parsing fixtures/small.ml > baseline
  $ compiler --parse fixtures/small.ml > actual.out
  $ cmp baseline actual.out
  $ compiler --parse --stop-after parsing -parsing topdown < fixtures/small.ml > actual.out
  $ cmp baseline actual.out
  $ compiler --stop-after parsing --parse fixtures/small.ml > actual.out
  $ cmp baseline actual.out
  $ printf '1 + 2\n' | compiler --parse-expression > expression.expected
  $ printf '1 + 2\n' | compiler --parse-expression --stop-after parsing -parsing topdown > actual.out
  $ cmp expression.expected actual.out
  $ printf '1 + 2\n' | compiler --stop-after parsing --parse-expression > actual.out
  $ cmp expression.expected actual.out
  $ compiler --stop-after parsing fixtures/recursive_value.ml > actual.out
  $ test -s actual.out
  $ compiler --stop-after anf --stop-after parsing fixtures/small.ml > actual.out
  $ cmp baseline actual.out

Usage errors are rejected before input is opened, in either conflict order.

  $ usage_error --stop-after codegen
  $ grep -q 'parsing' actual.err && grep -q 'anf' actual.err
  $ usage_error -parsing unknown
  $ grep -q 'topdown' actual.err && grep -q 'menhir' actual.err
  $ usage_error --stop-after
  $ usage_error -parsing
  $ usage_error -o
  $ usage_error --unknown
  $ usage_error missing.ml other.ml
  $ grep -q 'at most one input file' actual.err
  $ usage_error --parse --parse-expression
  $ usage_error --parse-expression --parse
  $ usage_error --parse --stop-after anf missing.ml
  $ grep -q 'conflict' actual.err
  $ usage_error --stop-after anf --parse missing.ml
  $ usage_error --parse-expression --stop-after anf missing.ml
  $ usage_error --stop-after anf --parse-expression missing.ml

Help does not read the missing file or run the demo.

  $ compiler missing.ml --help > help.txt 2> actual.err
  $ test ! -s actual.err && test ! -e out.ll
  $ grep -q -- '--stop-after' help.txt
  $ grep -q -- '-parsing' help.txt
  $ grep -q -- '-o' help.txt
  $ grep -q 'default: anf' help.txt
  $ grep -q 'default: menhir' help.txt

No arguments still emit the LLVM demo, without invoking an external C compiler.

  $ compiler > actual.out 2> actual.err
  $ test ! -s actual.out && test ! -s actual.err
  $ grep -q 'define i64 @main()' out.ll
  $ grep -q 'call void @print_int(i64 70)' out.ll

The parser/stage/output matrix writes exactly the same bytes to a file or
stdout, overwrites old contents, and leaves stdout empty for file output.

  $ for stage in parsing anf; do
  >   for parser in menhir topdown; do
  >     compiler --stop-after "$stage" -parsing "$parser" fixtures/small.ml > baseline || exit 1
  >     compiler fixtures/small.ml --stop-after "$stage" -parsing "$parser" -o - > actual.out || exit 1
  >     cmp baseline actual.out || exit 1
  >     printf 'old output\n' > result.txt
  >     compiler -o result.txt --stop-after "$stage" -parsing "$parser" fixtures/small.ml > actual.out 2> actual.err || exit 1
  >     test ! -s actual.out && test ! -s actual.err && cmp baseline result.txt || exit 1
  >     compiler --stop-after "$stage" -parsing "$parser" -o result.txt < fixtures/small.ml > actual.out || exit 1
  >     test ! -s actual.out && cmp baseline result.txt || exit 1
  >   done
  > done
  $ compiler -o result.txt < fixtures/small.ml > actual.out
  $ test ! -s actual.out && cmp baseline result.txt
  $ compiler --parse -o result.txt -parsing topdown fixtures/small.ml > actual.out
  $ compiler --stop-after parsing fixtures/small.ml > baseline
  $ test ! -s actual.out && cmp baseline result.txt
  $ printf '1 + 2\n' | compiler --parse-expression -parsing topdown -o result.txt > actual.out
  $ test ! -s actual.out && cmp expression.expected result.txt
  $ compiler fixtures/small.ml -o unused.txt -o result.txt > actual.out
  $ test ! -s actual.out && test ! -e unused.txt
  $ compiler fixtures/small.ml > baseline
  $ cmp baseline result.txt

The full source is consumed before an output path is opened, including when
the input and output paths are the same.

  $ cp fixtures/small.ml input-output.txt
  $ chmod u+w input-output.txt
  $ compiler input-output.txt -o input-output.txt > actual.out
  $ test ! -s actual.out && cmp baseline input-output.txt

Syntax errors retain source positions for both parsers and both stages.
They neither overwrite an existing destination nor create a new one.

  $ printf 'preserve me\n' > original.txt
  $ cp original.txt result.txt
  $ for stage in parsing anf; do
  >   for parser in menhir topdown; do
  >     runtime_error --stop-after "$stage" -parsing "$parser" -o result.txt fixtures/invalid.ml || exit 1
  >     grep -q 'line 2, column 1' actual.err && cmp original.txt result.txt || exit 1
  >     runtime_error --stop-after "$stage" -parsing "$parser" -o new-output.txt fixtures/invalid.ml || exit 1
  >     test ! -e new-output.txt || exit 1
  >   done
  > done

Valid syntax with an unsupported recursive RHS fails only during ANF,
including the default stage, and preserves output just like syntax errors.

  $ for parser in menhir topdown; do
  >   runtime_error --stop-after anf -parsing "$parser" fixtures/recursive_value.ml || exit 1
  >   grep -q 'must have a lambda RHS' actual.err || exit 1
  >   runtime_error -parsing "$parser" -o result.txt fixtures/recursive_value.ml || exit 1
  >   cmp original.txt result.txt || exit 1
  >   runtime_error --stop-after anf -parsing "$parser" -o new-output.txt fixtures/recursive_value.ml || exit 1
  >   test ! -e new-output.txt || exit 1
  > done

I/O failures use stderr and status 1; usage failures happen before any file
is read or overwritten. Help also avoids creating the requested output.

  $ runtime_error missing.ml -o result.txt
  $ grep -q 'missing.ml' actual.err && cmp original.txt result.txt
  $ runtime_error fixtures/small.ml -o missing-dir/result.txt
  $ grep -q 'missing-dir/result.txt' actual.err
  $ usage_error --parse --stop-after anf missing.ml -o result.txt
  $ grep -q 'conflict' actual.err && cmp original.txt result.txt
  $ compiler missing.ml -o help-output.txt --help > help.txt 2> actual.err
  $ test ! -s actual.err && test ! -e help-output.txt
