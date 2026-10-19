  $ clang-18 -c runtime.c -o runtime.o
  $ ../compiler.exe --help
  Usage: compiler.exe [options] [file]
  Compiles an ML program from <file> (or stdin).
  
  Options:
    -parsing topdown|menhir   Parser to use: top-down (default) or bottom-up
    --stop-after parsing|anf|closure|lifting  Stop after the stage and print its result (default: anf)
    -o <file>                 Write the result to <file>; '-o -' writes to stdout (default)
    --extended                Allow reserved words as identifiers (e.g. `let in = 1 in in`); top-down parser only
    --ast                     With '--stop-after parsing', also print the AST
    --llvm-demo               Run the LLVM codegen demo instead of compiling (writes out.ll)
    --help                    Show this message


  $ ../compiler.exe --llvm-demo

  $ cat out.ll | grep -E 'source_filename|target datalayout|ModuleID' --invert-match
  target triple = "x86_64-pc-linux-gnu"
  
  declare void @print_int(i64)
  
  define i64 @main() {
  entry:
    call void @print_int(i64 70)
    ret i64 0
  }






  $ clang-18 out.ll runtime.o -o shmel1.exe
  $ echo "Press $(./shmel1.exe) to pay respect"
  Press F to pay respect

CPS-style factorial (task 1 requires the grammar to be rich enough for
this; evaluating it is out of scope here, only parsing is), checked on
both parsers:

  $ echo "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5" | ../compiler.exe --stop-after parsing
  (let rec fact n = (if (n <= 1) then 1 else (n * (fact (n - 1)))) in (fact 5))

  $ echo "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5" | ../compiler.exe -parsing menhir --stop-after parsing
  (let rec fact n = (if (n <= 1) then 1 else (n * (fact (n - 1)))) in (fact 5))

  $ echo "let in = 1 in in" | ../compiler.exe --extended --stop-after parsing
  (let in = 1 in in)

  $ echo "1 + 2" | ../compiler.exe --ast --stop-after parsing
  [(DExpr (BinOp (Add, (Const 1), (Const 2))))]
  (1 + 2)
