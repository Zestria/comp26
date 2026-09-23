  $ clang-18 -c runtime.c -o runtime.o
  $ ../compiler.exe --help
  Usage: compiler.exe [options] [file]
  Reads an expression from <file> (or stdin) and parses it.
  
  Options:
    --parser=comb|menhir  Select top-down (default) or bottom-up parser
    --extended            Allow reserved words as identifiers (e.g. `let in = 1 in in`); combinator parser only
    --ast                 Print the parsed AST
    --llvm-demo           Run the LLVM codegen demo instead of parsing (writes out.ll)
    --help                Show this message


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

  $ echo "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5" | ../compiler.exe
  (let rec fact n = (if (n <= 1) then 1 else (n * (fact (n - 1)))) in (fact 5))

  $ echo "let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5" | ../compiler.exe --parser=menhir
  (let rec fact n = (if (n <= 1) then 1 else (n * (fact (n - 1)))) in (fact 5))

  $ echo "let in = 1 in in" | ../compiler.exe --extended
  (let in = 1 in in)

  $ echo "1 + 2" | ../compiler.exe --ast
  (1 + 2)
  (BinOp (Add, (Const 1), (Const 2)))
