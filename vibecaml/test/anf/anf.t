Подготовленные вручную программы после closure conversion и lambda lifting.
Печать фиксирует форму вызовов, условия и число временных переменных.

  $ ./fixture.exe --stats fack.src
  let: 9 -> 3
  $ ./fixture.exe --stats fibk.src
  let: 12 -> 5
  $ ./fixture.exe fack.src
  let rec fack_step step_k step_n step_m = let t__1 = ( * ) step_m step_n in
                                           step_k t__1
  and fack n k = if n <= 1 then (
                   k 1
                 ) else (
                   let t__4 = fack_step k n in
                   let t__5 = ( - ) n 1 in
                   fack t__5 t__4
                 )
  let identity x = x
  let result = fack 5 identity
  let value__9 = print_int result
  $ ./fixture.exe fibk.src
  let rec fibk_step_b k_b a_b b_b = let t__1 = ( + ) a_b b_b in
                                    k_b t__1
  and fibk_step_a n_a k_a a_a = let t__3 = fibk_step_b k_a a_a in
                                let t__4 = ( - ) n_a 2 in
                                fibk t__4 t__3
  and fibk n k = if n < 2 then (
                   k n
                 ) else (
                   let t__7 = fibk_step_a n k in
                   let t__8 = ( - ) n 1 in
                   fibk t__8 t__7
                 )
  let identity x = x
  let result = fibk 10 identity
  let value__12 = print_int result

Для каждого примера исполняем исходник, сырую ANF и уменьшенную ANF. Сверяем
весь stdout: так ловятся и неверные числа, и изменение порядка print_int.

  $ for sample in fack fibk patterns branches arity effects operators discarded; do
  >   ocaml -w -a "$sample.src" > source.out || exit 1
  >   ./fixture.exe --raw "$sample.src" > raw.ml || exit 1
  >   ocaml -w -a raw.ml > raw.out || exit 1
  >   ./fixture.exe "$sample.src" > small.ml || exit 1
  >   ocaml -w -a small.ml > small.out || exit 1
  >   diff -u source.out raw.out || exit 1
  >   diff -u source.out small.out || exit 1
  >   printf '%s: ' "$sample"
  >   cat small.out
  >   printf '\n'
  > done
  fack: 120
  fibk: 55
  patterns: 615
  branches: 14
  arity: 305799
  effects: 215436728
  operators: 1485-5
  discarded: 5
