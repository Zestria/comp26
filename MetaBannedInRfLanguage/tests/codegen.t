Tuple values, patterns, and closures executed on RISC-V.

  $ compile () {
  >   ../driver/compiler.exe --stop-after riscv -o "$1.s" "$1.ml" &&
  >   riscv64-linux-gnu-gcc -march=rv64gc -mabi=lp64d -static "$1.s" runtime_riscv.o -o "$1.exe"
  > }
  $ run () { timeout --kill-after=1s 10s qemu-riscv64 "./$1.exe" && printf '\n'; }

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

Partially applied user functions keep supplying arguments through fresh
closures, including when the function captures an outer variable.

  $ cat > partial.ml <<EOF
  > let add3 a b c = a + b + c;;
  > let add10 = add3 10;;
  > let add30 = add10 20;;
  > let () = print_int (add30 12);;
  > let () = print_int (add3 1 2 39)
  > EOF
  $ compile partial
  $ run partial
  4242

  $ cat > partial_capture.ml <<EOF
  > let make n =
  >   let rec go a b = n + a + b in
  >   go;;
  > let add10 = make 10;;
  > let add30 = add10 20;;
  > let () = print_int (add30 12)
  > EOF
  $ compile partial_capture
  $ run partial_capture
  42

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
  $ riscv64-linux-gnu-gcc -march=rv64gc -mabi=lp64d -static cases_menhir.s runtime_riscv.o -o cases_menhir.exe
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
  $ timeout --kill-after=1s 10s qemu-riscv64 ./match_failure.exe
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
  $ timeout --kill-after=1s 10s qemu-riscv64 ./failure.exe
  match failure: no case matched
  [1]

Plain (non-CPS) recursion returns values directly instead of through a
continuation.

  $ cat > plain.ml <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1);;
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);;
  > let () = print_int (fac 6);;
  > let () = print_int (fib 10)
  > EOF
  $ compile plain
  $ run plain
  72055

Closures passed as arguments are applied indirectly.

  $ cat > higher_order.ml <<EOF
  > let apply f x = f x;;
  > let twice f x = f (f x);;
  > let inc x = x + 1;;
  > let () = print_int (apply inc 41);;
  > let () = print_int (twice inc 40)
  > EOF
  $ compile higher_order
  $ run higher_order
  4242

Ten arguments force the last two through the stack; ten captured variables
force the closure environment beyond the register file.

  $ cat > many_args.ml <<EOF
  > let sum10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j;;
  > let () = print_int (sum10 1 2 3 4 5 6 7 8 6 0)
  > EOF
  $ compile many_args
  $ run many_args
  42

  $ cat > many_captures.ml <<EOF
  > let make a b c d e f g h i j = fun x -> a + b + c + d + e + f + g + h + i + j + x;;
  > let f = make 1 2 3 4 5 6 7 8 9 10;;
  > let () = print_int (f (-13))
  > EOF
  $ compile many_captures
  $ run many_captures
  42

Division truncates toward zero like OCaml, and a zero divisor reaches the
runtime handler.

  $ cat > division.ml <<EOF
  > let () = print_int (-7 / 3);;
  > let () = print_int (7 / -3);;
  > let () = print_int (-7 / -3)
  > EOF
  $ compile division
  $ run division
  -2-22

  $ cat > division_zero.ml <<EOF
  > let () = print_int (42 / 0)
  > EOF
  $ compile division_zero
  $ timeout --kill-after=1s 10s qemu-riscv64 ./division_zero.exe
  division by zero
  [1]

Boolean operators short-circuit, so the unevaluated division never runs.

  $ cat > logic.ml <<EOF
  > let () = print_int (if false && (1 / 0 = 0) then 1 else 42);;
  > let () = print_int (if true || (1 / 0 = 0) then 42 else 1);;
  > let () = print_int (if 3 < 5 && not (2 > 4) then 42 else 0)
  > EOF
  $ compile logic
  $ run logic
  424242

Matching on a plain integer uses the same constant patterns as tuple fields.

  $ cat > int_match.ml <<EOF
  > let f x = match x with | 0 -> 42 | _ -> x;;
  > let () = print_int (f 0);;
  > let () = print_int (f 7)
  > EOF
  $ compile int_match
  $ run int_match
  427

Closures nested in closures keep the intermediate environment alive.

  $ cat > nested_closures.ml <<EOF
  > let make a b = let c = a + b in fun x -> fun y -> x + y + c;;
  > let f = make 10 20;;
  > let g = f 5;;
  > let () = print_int (g 7)
  > EOF
  $ compile nested_closures
  $ run nested_closures
  42

  $ cat > closure_of_closure.ml <<EOF
  > let add a b = a + b;;
  > let make n = let f = add n in fun x -> f x;;
  > let () = print_int (make 40 2)
  > EOF
  $ compile closure_of_closure
  $ run closure_of_closure
  42

Local recursive functions are lifted and still capture their surroundings.

  $ cat > local_rec.ml <<EOF
  > let f n =
  >   let rec go acc i = if i = 0 then acc else go (acc + i) (i - 1) in
  >   go 0 n;;
  > let () = print_int (f 9)
  > EOF
  $ compile local_rec
  $ run local_rec
  45

Over-application consumes the closure result and keeps applying.

  $ cat > over_apply.ml <<EOF
  > let f a = fun b -> a + b;;
  > let () = print_int (f 40 2)
  > EOF
  $ compile over_apply
  $ run over_apply
  42

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
