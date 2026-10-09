[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
module CC = Middleend.Closure_conversion

let parse s =
  match Top_down.Parser.parse_expr s with
  | Ok e -> e
  | Error err -> raise (Failure (Top_down.Parser.string_of_error err))
;;

let converted_of_string s = CC.closure_convert (parse s)
let print_converted s = print_endline (string_of_expr (converted_of_string s))

(* ----------------------------------------------------------------- *)
(* Invariant: every [Abstr] is closed                                *)
(* ----------------------------------------------------------------- *)

let rec closed_expr env = function
  | Abstr (p, body) ->
    let fvs = CC.free_vars body in
    List.for_all (fun v -> String.equal v p || List.mem v env) fvs
    && closed_expr (p :: env) body
  | App (e1, e2) -> closed_expr env e1 && closed_expr env e2
  | If (c, t, e) -> closed_expr env c && closed_expr env t && closed_expr env e
  | Let (Recursive, name, rhs, rest) ->
    closed_expr (name :: env) rhs && closed_expr (name :: env) rest
  | Let (NotRecursive, name, rhs, rest) ->
    closed_expr env rhs && closed_expr (name :: env) rest
  | Ident _ | Const _ -> true
;;

let check_invariant source =
  let converted = converted_of_string source in
  if closed_expr [] converted
  then true
  else (
    Format.eprintf
      "Closure invariant violated for:\n%s\n\nResult:\n%s\n"
      source
      (string_of_expr converted);
    false)
;;

(* ----------------------------------------------------------------- *)
(* Substring search over AST                                         *)
(* ----------------------------------------------------------------- *)

let string_contains haystack needle =
  let nh = String.length haystack and nn = String.length needle in
  let rec go i =
    if i + nn > nh then false
    else if String.sub haystack i nn = needle then true
    else go (i + 1)
  in
  nn = 0 || go 0
;;

let rec exists_name_substr substr = function
  | Ident x -> string_contains x substr
  | Const _ -> false
  | App (e1, e2) -> exists_name_substr substr e1 || exists_name_substr substr e2
  | If (c, t, e) ->
    exists_name_substr substr c
    || exists_name_substr substr t
    || exists_name_substr substr e
  | Abstr (_, body) -> exists_name_substr substr body
  | Let (_, name, rhs, rest) ->
    string_contains name substr
    || exists_name_substr substr rhs
    || exists_name_substr substr rest
;;

(* ----------------------------------------------------------------- *)
(* expect-tests                                                      *)
(* ----------------------------------------------------------------- *)

let%expect_test "closed lambda is left alone" =
  print_converted "let f = fun x -> x + 1 in f 5";
  [%expect {| let f = fun x -> x + 1 in (f 5) |}]
;;

let%expect_test "captured variable becomes a leading parameter" =
  print_converted "let a = 1 in let f = fun x -> x + a in f 5";
  [%expect {|
    let a = 1 in
      let f = let clo_1 = fun a -> fun x -> x + a in (clo_1 a) in (f 5)
    |}]
;;

let%expect_test "multiple captures keep a deterministic order" =
  print_converted "let a = 1 in let b = 2 in let f = fun x -> x + a + b in f 3";
  [%expect {|
    let a = 1 in
      let b = 2 in
        let f = let clo_1 = fun a -> fun b -> fun x -> x + a + b in ((clo_1 a) b)
          in (f 3)
    |}]
;;

let%expect_test "bare lambda in expression position" =
  print_converted "let g = fun y -> y in g (fun x -> x + 1)";
  [%expect {| let g = fun y -> y in (g (fun x -> x + 1)) |}]
;;

let%expect_test "recursive function with captured variable" =
  print_converted
    "let n = 10 in let rec loop i = if i > n then 0 else i + loop (i + 1) in loop 1";
  [%expect {|
    let n = 10 in
      let rec loop = fun n -> fun i -> if i > n then 0 else i + ((loop n) i + 1)
        in let clo_1 = (loop n) in (clo_1 1)
    |}]
;;

let%expect_test "closure with captures used as a value" =
  print_converted "let a = 1 in let f = fun x -> x + a in let g = f in g 5";
  [%expect {|
    let a = 1 in
      let f = let clo_1 = fun a -> fun x -> x + a in (clo_1 a) in
        let g = f in (g 5)
    |}]
;;

let%expect_test "fresh names do not collide with source names" =
  print_converted "let f_clo0 = 42 in let a = 1 in let f = fun x -> x + a in f 5";
  [%expect {|
    let f_clo0 = 42 in
      let a = 1 in
        let f = let clo_1 = fun a -> fun x -> x + a in (clo_1 a) in (f 5)
    |}]
;;

let cps_fact =
  {|
let rec fact n k =
  if n = 0 then k 1
  else fact (n - 1) (fun acc -> k (n * acc))
in
fact 5 (fun x -> x)
|}
;;

let%expect_test "cps factorial" =
  print_converted cps_fact;
  [%expect {|
    let rec fact =
      fun n -> fun k -> if n = 0 then (k 1) else
                          ((fact n - 1) (let clo_1 =
                                           fun k -> fun n -> fun acc -> (k
                                                                        n * acc)
                                           in ((clo_1 k) n)))
      in ((fact 5) (fun x -> x))
    |}]
;;

let cps_fib =
  {|
let rec fib n k =
  if n < 2 then k n
  else fib (n - 1) (fun x -> fib (n - 2) (fun y -> k (x + y)))
in
fib 6 (fun x -> x)
|}
;;

let%expect_test "cps fibonacci" =
  print_converted cps_fib;
  [%expect {|
    let rec fib =
      fun n -> fun k -> if n < 2 then (k n) else
                          ((fib n - 1) (let clo_2 =
                                          fun fib -> fun k -> fun n -> fun x ->
                                                                        (
                                                                        (fib
                                                                        n - 2) (
                                                                        let clo_1 =
                                                                        fun k ->
                                                                        fun x ->
                                                                        fun y ->
                                                                        (k
                                                                        x + y) in
                                                                        (
                                                                        (clo_1 k) x)))
                                          in (((clo_2 fib) k) n)))
      in ((fib 6) (fun x_1 -> x_1))
    |}]
;;

(* ----------------------------------------------------------------- *)
(* Structural tests                                                  *)
(* ----------------------------------------------------------------- *)

let%test "closed lambda invariant holds on all basic examples" =
  List.for_all
    check_invariant
    [ "let f = fun x -> x + 1 in f 5"
    ; "let a = 1 in let f = fun x -> x + a in f 5"
    ; "let a = 1 in let b = 2 in let f = fun x -> x + a + b in f 3"
    ; "let g = fun y -> y in g (fun x -> x + 1)"
    ; "let n = 10 in let rec loop i = if i > n then 0 else i + loop (i + 1) in loop 1"
    ; cps_fact
    ; cps_fib
    ]
;;

let%test "closed lambda invariant holds on nested functions" =
  check_invariant
    "let a = 1 in let outer = fun x -> \
     let inner = fun y -> x + y + a in inner 3 in outer 5"
;;

let%test "closed lambda invariant holds on shadowed names" =
  List.for_all
    check_invariant
    [ "let x = 1 in let f = fun x -> x + 1 in f 5"
    ; "let f = fun x -> x + 1 in let f = fun y -> y + 2 in f 5"
    ; "let a = 1 in let f = fun a -> a + 1 in f 5"
    ]
;;

let%test "no wrapper when there are no captures" =
  let c = converted_of_string "let f = fun x -> x + 1 in f 5" in
  not (exists_name_substr "clo_" c)
;;

let%test "wrapper appears when there are captures" =
  let c = converted_of_string "let a = 1 in let f = fun x -> x + a in f 5" in
  exists_name_substr "clo_" c
;;

let%test "fresh names avoid user collisions" =
  let c =
    converted_of_string "let f_clo0 = 42 in let a = 1 in let f = fun x -> x + a in f 5"
  in
  exists_name_substr "f_clo" c
;;

let%test "cps factorial has wrappers for its lambdas" =
  exists_name_substr "clo_" (converted_of_string cps_fact)
;;

let%test "cps fibonacci has wrappers for its lambdas" =
  exists_name_substr "clo_" (converted_of_string cps_fib)
;;

let%test "closed lambda has no wrapper" =
  not (exists_name_substr "clo_" (converted_of_string "let f = fun x -> x + 1 in f 5"))
;;

(* ----------------------------------------------------------------- *)
(* Determinism                                                       *)
(* ----------------------------------------------------------------- *)

let cases =
  [ "let a = 1 in let f = fun x -> x + a in f 5"
  ; "let a = 1 in let b = 2 in let f = fun x -> x + a + b in f 3"
  ; "let g = fun y -> y in g (fun x -> x + 1)"
  ; "let a = 1 in let f = fun x -> x + a in let g = f in g 5"
  ; "let f_clo0 = 42 in let a = 1 in let f = fun x -> x + a in f 5"
  ; "let x = 1 in let f = fun x -> x + 1 in f 5"
  ; "let f = fun x -> x + 1 in let f = fun y -> y + 2 in f 5"
  ; "let a = 1 in let f = fun a -> a + 1 in f 5"
  ; "let a = 1 in let outer = fun x -> \
     let inner = fun y -> x + y + a in inner 3 in outer 5"
  ; "let n = 10 in let rec loop i = if i > n then 0 else i + loop (i + 1) in loop 1"
  ; cps_fact
  ; cps_fib
  ]
;;

let%test "conversion is deterministic" =
  List.for_all
    (fun source ->
       let a = string_of_expr (converted_of_string source) in
       let b = string_of_expr (converted_of_string source) in
       String.equal a b)
    cases
;;

let%test "invariant holds on all example cases" = List.for_all check_invariant cases
;;
