open Anf

type error =
  | Duplicate_binder of id
  | Unbound_local of id
  | Unbound_external of string

module Ids = Set.Make (Int)
module Names = Set.Make (String)

exception Invalid of error

let run ~externals action =
  let seen = ref Ids.empty in
  let externals = Names.of_list externals in
  let declare id =
    if Ids.mem id.stamp !seen then raise (Invalid (Duplicate_binder id));
    seen := Ids.add id.stamp !seen
  in
  let immediate scope = function
    | Constant _ | Builtin _ -> ()
    | Local id -> if not (Ids.mem id.stamp scope) then raise (Invalid (Unbound_local id))
    | External name ->
      if not (Names.mem name externals) then raise (Invalid (Unbound_external name))
  in
  let rec term scope = function
    | Result rhs -> compound scope rhs
    | Let (id, rhs, body) ->
      declare id;
      compound scope rhs;
      term (Ids.add id.stamp scope) body
    | Let_rec (fn, parameter, body, continuation) ->
      declare fn;
      let scope = Ids.add fn.stamp scope in
      declare parameter;
      term (Ids.add parameter.stamp scope) body;
      term scope continuation
  and compound scope = function
    | Value value -> immediate scope value
    | Apply (fn, arg) | Binary_primitive (_, fn, arg) ->
      immediate scope fn;
      immediate scope arg
    | Unary_primitive (_, arg) -> immediate scope arg
    | Lambda (parameter, body) ->
      declare parameter;
      term (Ids.add parameter.stamp scope) body
    | If (condition, yes, no) ->
      immediate scope condition;
      term scope yes;
      term scope no
  in
  let definition scope = function
    | Definition (id, rhs) ->
      declare id;
      term scope rhs;
      Ids.add id.stamp scope
    | Recursive_definition (fn, parameter, body) ->
      declare fn;
      let scope = Ids.add fn.stamp scope in
      declare parameter;
      term (Ids.add parameter.stamp scope) body;
      scope
  in
  try
    action term definition;
    Ok ()
  with
  | Invalid error -> Error error
;;

let term ~externals input = run ~externals (fun check _ -> check Ids.empty input)

let program ~externals input =
  run ~externals (fun _ definition -> ignore (List.fold_left definition Ids.empty input))
;;

let pp_error fmt = function
  | Duplicate_binder id -> Format.fprintf fmt "duplicate binder %s#%d" id.hint id.stamp
  | Unbound_local id -> Format.fprintf fmt "unbound local %s#%d" id.hint id.stamp
  | Unbound_external name -> Format.fprintf fmt "unbound external %S" name
;;
