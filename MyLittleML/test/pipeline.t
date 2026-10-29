$ ../compiler.exe --help
Usage: compiler.exe [OPTIONS] [FILE]
Middle-end for MyLittleML: parse an OCaml subset and run ANF,
closure conversion and lambda lifting.
Options:
-parsing topdown|menhir    choose the parser (default: topdown;
'bottomup' is accepted as an alias of 'menhir')
--parser topdown|bottomup  legacy alias of -parsing
--sticky                   enable sticky-keywords mode (top-down only)
--stop-after parsing|anf|closure-conversion|lambda-lifting
stop the pipeline after the given stage
(default: run the whole pipeline)
-o FILE                    write the result to FILE ('-o -' writes
to standard output, which is also the default)
--ast | --dump-ast         legacy: stop after parsing and print the AST
--help                     display this help and exit
If FILE is omitted, the program is read from standard input.

compiler: unrecognized option: --bogus
Try 'compiler.exe --help' for more information.
[2]

compiler: unknown parser: foo (expected 'topdown' or 'menhir')
Try 'compiler.exe --help' for more information.
[2]

compiler: cannot open file: does-not-exist.ml: No such file or directory
[1]

(let x  = 5 in ((x = 5)));;

(1 + ((2 * 3)));;

(let rec fact n k = (if ((n <= 1)) then ((k 1)) else ((((fact ((n - 1)))) (fun r -> ((k ((r * n)))))))));;

compiler: 1:9: syntax error
[1]

(let x  = 5 in ((x = 5)));;

(1 + ((2 * 3)));;

(let rec fact n k = (if ((n <= 1)) then ((k 1)) else ((((fact ((n - 1)))) (fun r -> ((k ((r * n)))))))));;

(let x  = 5 in ((x + 1)));;

(letx = 1);;

compiler: 1:9: syntax error
[1]

compiler: 1:11: keyword in cannot be used as an identifier
[1]

compiler: 1:5: keyword in cannot be used as an identifier
[1]

compiler: 1:16: unterminated comment
[1]

compiler: sticky mode is not supported by the bottom-up parser
[1]

(let x  = 1);;

(let rec f n = ((n + 1)));;

fun x -> ((x + 1)));;

(let ter  = 1);;

(letx = 1);;

compiler: sticky mode is not supported by the bottom-up parser
[1]

let x = 5 in (x + 1);;

let x = 5 in (x + 1);;

0

1

2

(let x  = 5 in ((x + 1)));;

let x = 5 in (x + 1);;

let x = 5 in (x + 1);;

compiler: unknown stage: bogus (expected 'parsing', 'anf', 'closure-conversion' or 'lambda-lifting')
Try 'compiler.exe --help' for more information.
[2]
