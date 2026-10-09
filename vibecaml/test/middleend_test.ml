[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml

let unwrap = function
  | Ok value -> value
  | Error message -> failwith message
;;

let () =
  Fresh.reset ();
  (* Имитируем имя предыдущего прохода: следующие проходы не должны сбросить
     общий счётчик и занять его номер повторно. *)
  let reserved = Fresh.name "reserved" in
  let source =
    unwrap
      (Rd_parser.parse_structure
         Config.default_extensions
         "let answer = ((fun first -> first + 1) 3, (fun second -> second * 2) 4)")
  in
  let renamed = unwrap (Rename.rename source) in
  let cc = unwrap (Closure_conv.convert renamed) in
  let lifted = unwrap (Lambda_lift.lift cc) in
  let anf = unwrap (Anf_conv.convert lifted) in
  let names = ref [ reserved ] in
  let add name = names := name :: !names in
  let rec expression = function
    | Anf.ALet (name, rhs, body) ->
      add name;
      action rhs;
      expression body
    | Anf.ACExpr rhs -> action rhs
  and action = function
    | Anf.CIf (_, yes, no) ->
      expression yes;
      expression no
    | _ -> ()
  in
  List.iter
    (fun (_, bindings) ->
       List.iter
         (fun (name, parameters, body) ->
            add name;
            List.iter add parameters;
            expression body)
         bindings)
    anf;
  assert (List.length !names = List.length (List.sort_uniq String.compare !names));
  let numbers =
    List.filter_map
      (fun name ->
         match String.split_on_char '_' name with
         | [ _; ""; number ] -> Some (int_of_string number)
         | _ -> None)
      !names
    |> List.sort Int.compare
  in
  (* Проверяем именно передачу состояния между проходами, а не конкретный
     порядок их временных переменных. Упрощение новых имён не создаёт. *)
  assert (List.length numbers > 3);
  assert (numbers = List.init (List.length numbers) (fun index -> index + 1));
  ignore (Anf_simplify.simplify anf);
  assert (Fresh.name "after" = "after__" ^ string_of_int (List.length numbers + 1))
;;
