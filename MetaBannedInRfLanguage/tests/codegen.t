Tuple values, patterns, and closures executed on RISC-V.

  $ riscv64-linux-gnu-gcc -O2 -Wall -Wextra -Werror -c ../backend/runtime.c -o runtime.o
  $ compile () {
  >   ../driver/compiler.exe --stop-after riscv -o "$1.s" "$1.ml" &&
  >   riscv64-linux-gnu-gcc -static "$1.s" runtime.o -o "$1.exe"
  > }
  $ run () { qemu-riscv64 "./$1.exe" && printf '\n'; }

A returned tuple contains tagged integers, booleans, and unit. Wildcard fields
are skipped when the receiving function destructures it.

  $ cat > fields.ml <<EOF
  > let make x = (x, true, (), x + 2);;
  > let sum (x, _, _, y) = x + y;;
  > let () = print_int (sum (make 20))
  > EOF
  $ compile fields
  $ run fields
  42

Nested tuple patterns in function parameters.

  $ cat > nested.ml <<EOF
  > let make x = ((x, 99), (true, (x + 2, ())));;
  > let sum ((x, _), (flag, (y, ()))) = if flag then x + y else 0;;
  > let () = print_int (sum (make 20))
  > EOF
  $ compile nested
  $ run nested
  42

Creating closures for tuple fields may clobber argument registers. Captured
functions, builtin operators, and the print_int adapter all survive packing.

  $ cat > functions.ml <<EOF
  > let make x = ((fun y -> x + y), (+), print_int);;
  > let (f, plus, output) = make 40;;
  > let inc = plus 1;;
  > let () = output (f (inc 1))
  > EOF
  $ compile functions
  $ run functions
  42

Each escaping closure retains its own tuple after the creator's frame returns.

  $ cat > capture.ml <<EOF
  > let make n = let pair = (n, n + 1) in fun x -> let (a, b) = pair in a + b + x;;
  > let f = make 20;;
  > let g = make 30;;
  > let () = print_int (f 1);;
  > let () = print_int (g 1);;
  > let () = print_int (f 3)
  > EOF
  $ compile capture
  $ run capture
  426244

Recursive functions construct and unpack tuple arguments on every step.

  $ cat > recursive.ml <<EOF
  > let rec loop (n, acc) = if n = 0 then acc else loop (n - 1, acc + n);;
  > let () = print_int (loop (5, 27))
  > EOF
  $ compile recursive
  $ run recursive
  42

Nested refutable patterns fall through to later cases without losing fields.

  $ cat > patterns.ml <<EOF
  > let choose pair = match pair with
  >   | (0, (true, x)) -> x
  >   | (_, (false, _)) -> -1
  >   | (_, (_, x)) -> x + 1
  > ;;
  > let () = print_int (choose (0, (true, 42)));;
  > let () = print_int (choose (7, (false, 100)));;
  > let () = print_int (choose (1, (true, 41)))
  > EOF
  $ compile patterns
  $ run patterns
  42-142

A tuple match checks fields from left to right and selects the first matching
case. The variable case must handle the value rejected by the constant case.

  $ cat > cases.ml <<EOF
  > let choose triple = match triple with
  >   | (true, 0, x) -> x
  >   | (true, n, x) -> n + x
  >   | (false, _, _) -> -1
  > ;;
  > let () = print_int (choose (true, 0, 42));;
  > let () = print_int (choose (true, 20, 22));;
  > let () = print_int (choose (false, 0, 42))
  > EOF
  $ compile cases
  $ run cases
  4242-1
  $ ../driver/compiler.exe -parsing menhir --stop-after riscv -o cases_menhir.s cases.ml
  $ riscv64-linux-gnu-gcc -static cases_menhir.s runtime.o -o cases_menhir.exe
  $ run cases_menhir
  4242-1

A wildcard can match the whole tuple after a more specific case fails.

  $ cat > wildcard.ml <<EOF
  > let choose pair = match pair with
  >   | (0, x) -> x
  >   | _ -> 42
  > ;;
  > let () = print_int (choose (0, 7));;
  > let () = print_int (choose (1, 99))
  > EOF
  $ compile wildcard
  $ run wildcard
  742

Names bound in different nested patterns may occupy different fields. They
shadow the function parameter only inside the selected match branch.

  $ cat > match_scope.ml <<EOF
  > let choose x pair =
  >   let result = match pair with
  >     | (0, (x, y)) -> x - y
  >     | (_, (y, x)) -> x - y
  >   in result + x
  > ;;
  > let () = print_int (choose 10 (0, (40, 8)));;
  > let () = print_int (choose 10 (1, (8, 40)))
  > EOF
  $ compile match_scope
  $ run match_scope
  4242

The function syntax uses the same tuple matching rules.

  $ cat > function_cases.ml <<EOF
  > let choose = function | (0, x) -> x | (_, x) -> x + 1;;
  > let () = print_int (choose (0, 42));;
  > let () = print_int (choose (1, 41))
  > EOF
  $ compile function_cases
  $ run function_cases
  4242

An incomplete explicit match fails when a nested constant does not match.
It must neither return a field from the rejected case nor run the continuation.

  $ cat > match_failure.ml <<EOF
  > let choose pair = match pair with | (0, (true, x)) -> x;;
  > let () = print_int (choose (0, (false, 42)));;
  > let () = print_int 99
  > EOF
  $ compile match_failure
  $ qemu-riscv64 ./match_failure.exe
  match failure: no case matched
  [1]

Equality compares contents of separately allocated nested tuples. It also
works through an operator value and a partially applied operator closure.

  $ cat > equality.ml <<EOF
  > let make x = (x, (x + 1, false), ());;
  > let eq = (=);;
  > let different = (<>);;
  > let same = eq (make 20);;
  > let bit b = if b then 1 else 0;;
  > let () = print_int (bit (make 20 = make 20));;
  > let () = print_int (bit (make 20 = make 21));;
  > let () = print_int (bit (different (make 20) (make 21)));;
  > let () = print_int (if same (make 20) then 42 else 0)
  > EOF
  $ compile equality
  $ run equality
  10142

Tuple-bound functions shadow existing names only within the binding's scope.

  $ cat > shadow.ml <<EOF
  > let f x = x + 1;;
  > let invoke pair = let (f, _) = pair in f 41;;
  > let () = print_int (invoke ((fun x -> x + 1), ()));;
  > let () = print_int (f 41)
  > EOF
  $ compile shadow
  $ run shadow
  4242

Large tuples exercise argument buffers and field offsets beyond 12 bits.
Only the last two fields are needed by the pattern.

  $ awk 'BEGIN { printf "let make x = ("; for (i=0; i<258; i++) printf "%d, ", i; print "x, x + 2);;"; printf "let sum ("; for (i=0; i<258; i++) printf "_, "; print "a, b) = a + b;;"; print "let () = print_int (sum (make 20))" }' > wide.ml
  $ compile wide
  $ run wide
  42

Failure in a tuple parameter reaches the runtime handler, not the continuation.

  $ cat > failure.ml <<EOF
  > let only_zero (0, x) = x;;
  > let () = print_int (only_zero (1, 42));;
  > let () = print_int 99
  > EOF
  $ compile failure
  $ qemu-riscv64 ./failure.exe
  match failure: no case matched
  [1]

The shared heap header also applies to closures. CPS programs still allocate
and apply captured continuations correctly after tuples were introduced.

  $ cat > fac.ml <<EOF
  > let rec fac n k = if n = 0 then k 1 else fac (n - 1) (fun r -> k (n * r));;
  > let identity x = x;;
  > let () = print_int (fac 6 identity)
  > EOF
  $ compile fac
  $ run fac
  720

  $ cat > fib.ml <<EOF
  > let rec fib n k =
  >   if n < 2 then k n
  >   else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
  > ;;
  > let identity x = x;;
  > let () = print_int (fib 8 identity)
  > EOF
  $ compile fib
  $ run fib
  21
