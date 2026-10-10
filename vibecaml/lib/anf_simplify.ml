[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf

let atom aliases = function
  | ImmId name as value ->
    (match List.assoc_opt name aliases with
     | Some value -> value
     | None -> value)
  | value -> value
;;

let condition aliases = function
  | CondImm value -> CondImm (atom aliases value)
  | CondCmp (operator, left, right) ->
    CondCmp (operator, atom aliases left, atom aliases right)
;;

let atom_uses name = function
  | ImmId other -> String.equal name other
  | ImmNum _ | ImmBool _ | ImmUnit -> false
;;

let rec uses name = function
  | ACExpr value -> computation_uses name value
  | ALet (_, value, body) -> computation_uses name value || uses name body

and computation_uses name = function
  | CImmExpr value | CField (value, _, _) -> atom_uses name value
  | CApp (function_, arguments) -> List.exists (atom_uses name) (function_ :: arguments)
  | CTuple (first, second, rest) -> List.exists (atom_uses name) (first :: second :: rest)
  | CIf (condition, yes, no) ->
    (match condition with
     | CondImm value -> atom_uses name value
     | CondCmp (_, left, right) -> atom_uses name left || atom_uses name right)
    || uses name yes
    || uses name no
;;

(* Без эффектов только сборка кортежа и чтение поля. Вызов может печатать,
   поэтому его результат сохраняется, даже если он не нужен. *)
let pure = function
  | CTuple _ | CField _ -> true
  | CImmExpr _ | CApp _ | CIf _ -> false
;;

let rec expression aliases = function
  | ACExpr value -> ACExpr (computation aliases value)
  | ALet (name, value, body) ->
    let value = computation aliases value in
    (match value with
     | CImmExpr value -> expression ((name, value) :: aliases) body
     | _ ->
       let body = expression aliases body in
       (match body with
        | ACExpr (CImmExpr (ImmId result)) when String.equal name result -> ACExpr value
        | _ when pure value && not (uses name body) -> body
        | _ -> ALet (name, value, body)))

and computation aliases = function
  | CImmExpr value -> CImmExpr (atom aliases value)
  | CApp (function_, arguments) ->
    CApp (atom aliases function_, List.map (atom aliases) arguments)
  | CTuple (first, second, rest) ->
    CTuple (atom aliases first, atom aliases second, List.map (atom aliases) rest)
  | CField (value, index, size) -> CField (atom aliases value, index, size)
  | CIf (value, yes, no) ->
    CIf (condition aliases value, expression aliases yes, expression aliases no)
;;

let simplify program =
  (* Глобальные объявления сохраняем: следующий проход и внешний main могут
     обращаться к ним, даже если здесь имя нигде не используется. *)
  List.map
    (fun (flag, bindings) ->
       ( flag
       , List.map
           (fun (name, parameters, body) -> name, parameters, expression [] body)
           bindings ))
    program
;;
