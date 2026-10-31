[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format

type ('function_, 'callee) printers =
  { identifier : formatter -> Id.t -> unit
  ; atom : formatter -> Anf.atom -> unit
  ; term : formatter -> ('function_, 'callee) Anf.term -> unit
  }

let separated separator print =
  pp_print_list ~pp_sep:(fun formatter () -> fprintf formatter "%s@ " separator) print
;;

module Names = struct
  type t =
    { names : string Id.Map.t
    ; next : int
    }

  let empty = { names = Id.Map.empty; next = 0 }

  let add state id =
    if Id.Map.mem id state.names
    then state
    else (
      let hint = Id.name id in
      let hint =
        if
          String.length hint > 0
          && Bkml_frontend.Name.Syntax.is_identifier_start hint.[0]
          && String.for_all Bkml_frontend.Name.Syntax.is_identifier_char hint
        then hint
        else "v"
      in
      let name = Printf.sprintf "%s_%d" hint state.next in
      { names = Id.Map.add id name state.names; next = state.next + 1 })
  ;;

  let add_all = List.fold_left add

  let atom state = function
    | Anf.Var id -> add state id
    | Constant _ | Primitive _ -> state
  ;;

  let find state id = Id.Map.find id state.names
end

let names ~callee ~function_ =
  Anf.fold ~callee ~atom:Names.atom ~binder:Names.add ~function_
;;

let make names ~callee:print_callee print_function =
  let identifier formatter id = pp_print_string formatter (Names.find names id) in
  let atom formatter = function
    | Anf.Constant (Int literal) ->
      let text = Bkml_frontend.Integer_literal.to_string literal in
      if String.starts_with ~prefix:"-" text
      then fprintf formatter "(%s)" text
      else pp_print_string formatter text
    | Constant (Bool b) -> pp_print_bool formatter b
    | Constant Unit -> pp_print_string formatter "()"
    | Var id -> identifier formatter id
    | Primitive primitive -> fprintf formatter "(%s)" (Primitive.name primitive)
  in
  let callee = print_callee identifier atom in
  let rec term formatter = function
    | Anf.Return computation -> value formatter computation
    | Let (id, computation, body) ->
      fprintf
        formatter
        "@[<v>let %a = %a in@,%a@]"
        identifier
        id
        value
        computation
        term
        body
    | Let_rec (id, rhs, body) ->
      fprintf
        formatter
        "@[<v>let rec %a = (@[<v>%a@]) in@,%a@]"
        identifier
        id
        term
        rhs
        term
        body
    | Sequence (computation, body) ->
      fprintf formatter "@[<v>let _ = %a in@,%a@]" value computation term body
  and value formatter = function
    | Anf.Atom a -> atom formatter a
    | Tuple (a, b, rest) ->
      fprintf formatter "@[<hov>(%a)@]" (separated "," atom) (a :: b :: rest)
    | Field (tuple, index) -> fprintf formatter "field[%d] %a" index atom tuple
    | Function definition -> print_function identifier atom term formatter definition
    | Apply (f, a, rest) ->
      fprintf formatter "@[<hov>%a %a@]" callee f (separated "" atom) (a :: rest)
    | If (condition, yes, no) ->
      fprintf
        formatter
        "@[<v>if %a then (@[<v>%a@])@,else (@[<v>%a@])@]"
        atom
        condition
        term
        yes
        term
        no
  in
  { identifier; atom; term }
;;

let expression formatter expression =
  let callee_names state value ~arity:_ = Names.atom state value in
  let rec function_names state (definition : Anf.function_definition) =
    let state = Names.add_all state (definition.parameter :: definition.parameters) in
    names ~callee:callee_names ~function_:function_names state definition.body
  in
  let names =
    names ~callee:callee_names ~function_:function_names Names.empty expression
  in
  let printers =
    make
      names
      ~callee:(fun _identifier atom -> atom)
      (fun identifier _atom term formatter (definition : Anf.function_definition) ->
         fprintf
           formatter
           "@[<hov 2>fun %a ->@ %a@]"
           (separated "" identifier)
           (definition.parameter :: definition.parameters)
           term
           definition.body)
  in
  printers.term formatter expression
;;

let program = expression
