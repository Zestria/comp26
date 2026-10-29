[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml
open Anf
open Riscv

let unwrap = function
  | Ok value -> value
  | Error message ->
    prerr_endline message;
    exit 1
;;

let value body = [ Ast.Non_recursive, [ "result", [], body ] ]
let generate body = unwrap (Codegen.generate (value body))

let instruction expected =
  List.exists (function
    | Instr (actual, _) -> actual = expected
    | _ -> false)
;;

let () =
  let bad action = assert (Result.is_error (Codegen.generate (value (ACExpr action)))) in
  bad (CApp (ImmId "+.", [ ImmNum 1; ImmNum 2 ]));
  bad (CImmExpr (ImmId "undefined"));
  bad (CApp (ImmNum 1, [ ImmNum 2 ]));
  bad (CField (ImmNum 0, -1, 2));
  bad (CField (ImmNum 0, 2, 2));
  bad (CField (ImmNum 0, 256, 300));
  bad (CTuple (ImmUnit, ImmUnit, List.init 254 (fun _ -> ImmUnit)));
  let rec lets count body =
    if count = 0
    then body
    else ALet ("local" ^ string_of_int count, CImmExpr ImmUnit, lets (count - 1) body)
  in
  assert (Result.is_error (Codegen.generate (value (lets 300 (aimm ImmUnit)))));
  let duplicate =
    ALet ("x", CImmExpr ImmUnit, ALet ("x", CImmExpr ImmUnit, aimm ImmUnit))
  in
  assert (Result.is_error (Codegen.generate (value duplicate)));
  ignore (unwrap (Codegen.generate []));
  (* Принтер содержит assert для imm12. Любой принятый кодогеном кадр
     должен печататься без исключения, включая граничные литералы. *)
  List.iter
    (fun number ->
       ignore (Format.asprintf "%a" pp_program (generate (aimm (ImmNum number)))))
    [ min_int; max_int; 0; -1 ]
;;

let () =
  let parameters = List.init 9 (fun n -> "arg" ^ string_of_int n) in
  let program =
    [ Ast.Non_recursive, [ "nine", parameters, avar "arg8" ]
    ; ( Ast.Non_recursive
      , [ "answer", [], ACExpr (CApp (ImmId "nine", List.init 9 (fun n -> ImmNum n))) ] )
    ]
    |> Codegen.generate
    |> unwrap
  in
  assert (instruction (Sd (T0, 0, Sp)) program);
  assert (instruction (Ld (T0, 0, S0)) program);
  assert (
    not
      (List.exists
         (function
           | Instr (Call name, _) -> (name :> string) = "vc_apply"
           | _ -> false)
         program));
  let count predicate = List.length (List.filter predicate program) in
  assert (
    count (function
      | Instr (Sd (Ra, _, Sp), _) -> true
      | _ -> false)
    = 2);
  assert (
    count (function
      | Instr (Ld (Ra, _, Sp), _) -> true
      | _ -> false)
    = 2);
  assert (
    List.for_all
      (function
        | Instr (Addi (Sp, Sp, bytes), _) -> bytes mod 16 = 0
        | _ -> true)
      program)
;;

let () =
  let body =
    ALet
      ( "chosen"
      , CIf (CondImm (ImmBool true), aimm (ImmNum 1), aimm (ImmNum 2))
      , avar "chosen" )
  in
  let program = generate body in
  let stores =
    List.filter
      (function
        | Instr (Sd (A0, -24, S0), _) -> true
        | _ -> false)
      program
  in
  assert (List.length stores = 2);
  (* Общий хвост не копируется в ветки. Размер кода цепочки if линейный,
     а не просто результат вычисления совпадает с исходной программой. *)
  let rec chain n =
    if n = 0
    then aimm ImmUnit
    else
      ALet
        ( "if_result" ^ string_of_int n
        , CIf (CondImm (ImmBool true), aimm (ImmNum 1), aimm (ImmNum 2))
        , chain (n - 1) )
  in
  let size n = List.length (generate (chain n)) in
  assert (size 32 - size 16 = 2 * (size 16 - size 8))
;;
