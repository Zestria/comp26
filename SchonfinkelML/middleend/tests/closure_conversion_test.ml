[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Common
module Anf = Middleend.Anf
module CC = Middleend.Closure_conversion

let parse s =
  match Top_down.Parser.parse_expr s with
  | Ok e -> e
  | Error err -> raise (Failure (Top_down.Parser.string_of_error err))
;;

let converted_of_string s = CC.closure_convert (Anf.create_anf (parse s))
let print_converted s = print_endline (Anf.string_of_aexpr (converted_of_string s))

let rec closed_aexpr env = function
  | Anf.ACExpr (Anf.CAbstr (p, body)) ->
    let fvs = Anf.free_vars body in
    List.for_all (fun v -> String.equal v p || List.mem v env) fvs
    && closed_aexpr (p :: env) body
  | Anf.ACExpr (Anf.CIfExpr (_, t, e)) -> closed_aexpr env t && closed_aexpr env e
  | Anf.ACExpr _ -> true
  | Anf.ALet (flag, name, rhs, rest) ->
    let env_rhs = if flag = Recursive then name :: env else env in
    closed_aexpr env_rhs rhs && closed_aexpr (name :: env) rest
;;

let check_invariant source =
  let converted = converted_of_string source in
  if closed_aexpr [] converted
  then true
  else (
    Format.eprintf
      "Closure invariant violated for:\n%s\n\nResult:\n%s\n"
      source
      (Anf.string_of_aexpr converted);
    false)
;;

let string_contains haystack needle =
  let nh = String.length haystack
  and nn = String.length needle in
  let rec go i =
    if i + nn > nh
    then false
    else if String.sub haystack i nn = needle
    then true
    else go (i + 1)
  in
  nn = 0 || go 0
;;

let rec exists_name_substr substr = function
  | Anf.ACExpr c -> exists_name_substr_c substr c
  | Anf.ALet (_, name, rhs, rest) ->
    string_contains name substr
    || exists_name_substr substr rhs
    || exists_name_substr substr rest

and exists_name_substr_c substr = function
  | Anf.CAbstr (_, b) -> exists_name_substr substr b
  | Anf.CApp (f, a, args) ->
    has_substr substr f || has_substr substr a || List.exists (has_substr substr) args
  | Anf.CBinop (_, a, b) -> has_substr substr a || has_substr substr b
  | Anf.CIfExpr (i, t, e) ->
    has_substr substr i || exists_name_substr substr t || exists_name_substr substr e
  | Anf.CImmExpr i -> has_substr substr i

and has_substr substr = function
  | Anf.ImmId s -> string_contains s substr
  | _ -> false
;;

let%expect_test "closed lambda is left alone" =
  print_converted "let f = fun x -> x + 1 in f 5";
  [%expect {| let f = fun x -> x + 1 in (f 5) |}]
;;

let%expect_test "captured variable becomes a leading parameter" =
  print_converted "let a = 1 in let f = fun x -> x + a in f 5";
  [%expect
    {|
    let a = 1 in
      let f = fun a -> fun x -> x + a in let f_clo1 = (f a) in (f_clo1 5)
    |}]
;;

let%expect_test "multiple captures keep a deterministic order" =
  print_converted "let a = 1 in let b = 2 in let f = fun x -> x + a + b in f 3";
  [%expect
    {|
    let a = 1 in
      let b = 2 in
        let f = fun a -> fun b -> fun x -> let v1 = x + a in v1 + b in
          let f_clo1 = (f a b) in (f_clo1 3)
    |}]
;;

let%expect_test "bare lambda in expression position" =
  print_converted "let g = fun y -> y in g (fun x -> x + 1)";
  [%expect {| let g = fun y -> y in let v1 = fun x -> x + 1 in (g v1) |}]
;;

let%expect_test "recursive function with captured variable" =
  print_converted
    "let n = 10 in let rec loop i = if i > n then 0 else i + loop (i + 1) in loop 1";
  [%expect
    {|
    let n = 10 in
      let rec loop =
        fun n -> fun i -> let v1 = i > n in
                            if v1 then 0 else
                              let v2 = i + 1 in let v3 = (loop n v2) in i + v3
        in let loop_clo1 = (loop n) in (loop_clo1 1)
    |}]
;;

let%expect_test "closure with captures used as a value" =
  print_converted "let a = 1 in let f = fun x -> x + a in let g = f in g 5";
  [%expect
    {|
    let a = 1 in
      let f = fun a -> fun x -> x + a in
        let f_clo1 = (f a) in let g = f_clo1 in (g 5)
    |}]
;;

let%expect_test "fresh names do not collide with source names" =
  print_converted "let f_clo0 = 42 in let a = 1 in let f = fun x -> x + a in f 5";
  [%expect
    {|
    let f_clo0 = 42 in
      let a = 1 in
        let f = fun a -> fun x -> x + a in let f_clo1 = (f a) in (f_clo1 5)
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
  [%expect
    {|
    let rec fact =
      fun n -> fun k -> let v1 = n = 0 in
                          if v1 then (k 1) else
                            let v2 = n - 1 in
                              let v4 =
                                fun k -> fun n -> fun acc -> let v3 = n * acc in
                                                               (k v3)
                                in let v4_clo1 = (v4 k n) in (fact v2 v4_clo1)
      in let v5 = fun x -> x in (fact 5 v5)
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
  [%expect
    {|
    let rec fib =
      fun n -> fun k -> let v1 = n < 2 in
                          if v1 then (k n) else
                            let v2 = n - 1 in
                              let v6 =
                                fun fib -> fun k -> fun n -> fun x -> let v3 =
                                                                        n - 2 in
                                                                        let v5 =
                                                                        fun k ->
                                                                        fun x ->
                                                                        fun y ->
                                                                        let v4 =
                                                                        x + y in
                                                                        (k v4) in
                                                                        let v5_clo1 =
                                                                        (v5 k x)
                                                                        in
                                                                        (fib v3 v5_clo1)
                                in let v6_clo2 = (v6 fib k n) in (fib v2 v6_clo2)
      in let v7 = fun x -> x in (fib 6 v7)
    |}]
;;

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
    "let a = 1 in let outer = fun x -> let inner = fun y -> x + y + a in inner 3 in \
     outer 5"
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
  not (exists_name_substr "_clo" c)
;;

let%test "wrapper appears when there are captures" =
  let c = converted_of_string "let a = 1 in let f = fun x -> x + a in f 5" in
  exists_name_substr "_clo" c
;;

let%test "bare lambda with captures gets a wrapper" =
  let c = converted_of_string "let a = 1 in (fun x -> x + a)" in
  exists_name_substr "lam_clo" c
;;

let%test "bare lambda without captures keeps no wrapper" =
  let c = converted_of_string "let g = fun y -> y in g (fun x -> x + 1)" in
  not (exists_name_substr "lam_clo" c)
;;

let%test "fresh names avoid user collisions" =
  let c =
    converted_of_string "let f_clo0 = 42 in let a = 1 in let f = fun x -> x + a in f 5"
  in
  exists_name_substr "f_clo0" c && exists_name_substr "f_clo1" c
;;

let%test "free variables of every lambda are its leading parameters" =
  let rec check env = function
    | Anf.ACExpr (Anf.CAbstr (p, body)) ->
      let fvs = Anf.free_vars body in
      List.for_all (fun v -> String.equal v p || List.mem v env) fvs
      && check (p :: env) body
    | Anf.ACExpr (Anf.CIfExpr (_, t, e)) -> check env t && check env e
    | Anf.ACExpr _ -> true
    | Anf.ALet (flag, name, rhs, rest) ->
      let env_rhs = if flag = Recursive then name :: env else env in
      check env_rhs rhs && check (name :: env) rest
  in
  List.for_all
    (fun source -> check [] (converted_of_string source))
    [ cps_fact
    ; cps_fib
    ; "let n = 10 in let rec loop i = if i > n then 0 else i + loop (i + 1) in loop 1"
    ]
;;

let%test "cps factorial has wrappers for its lambdas" =
  exists_name_substr "_clo" (converted_of_string cps_fact)
;;

let%test "cps fibonacci has wrappers for its lambdas" =
  exists_name_substr "_clo" (converted_of_string cps_fib)
;;

let%test "closed lambda has no wrapper" =
  not (exists_name_substr "_clo" (converted_of_string "let f = fun x -> x + 1 in f 5"))
;;

let cases =
  [ "let a = 1 in let f = fun x -> x + a in f 5"
  ; "let a = 1 in let b = 2 in let f = fun x -> x + a + b in f 3"
  ; "let g = fun y -> y in g (fun x -> x + 1)"
  ; "let a = 1 in let f = fun x -> x + a in let g = f in g 5"
  ; "let f_clo0 = 42 in let a = 1 in let f = fun x -> x + a in f 5"
  ; "let x = 1 in let f = fun x -> x + 1 in f 5"
  ; "let f = fun x -> x + 1 in let f = fun y -> y + 2 in f 5"
  ; "let a = 1 in let f = fun a -> a + 1 in f 5"
  ; "let a = 1 in let outer = fun x -> let inner = fun y -> x + y + a in inner 3 in \
     outer 5"
  ; "let n = 10 in let rec loop i = if i > n then 0 else i + loop (i + 1) in loop 1"
  ; cps_fact
  ; cps_fib
  ]
;;

let%test "conversion is deterministic" =
  List.for_all
    (fun source ->
       let a = Anf.string_of_aexpr (converted_of_string source) in
       let b = Anf.string_of_aexpr (converted_of_string source) in
       String.equal a b)
    cases
;;

let%test "invariant holds on all example cases" = List.for_all check_invariant cases
