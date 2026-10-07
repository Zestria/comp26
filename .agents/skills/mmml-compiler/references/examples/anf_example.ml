(* Standalone teaching language: integer conditions, left-to-right evaluation.
   This is not the MMML AST or its boolean/type semantics. *)
type expr =
  | Num of int
  | Var of string
  | Add of expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr

type imm =
  | Int of int
  | Id of string

type compound =
  | Value of imm
  | Plus of imm * imm
  | Branch of imm * term * term

and term =
  | Return of imm
  | Bind of string * compound * term

module Names = Set.Make (String)

let rec names = function
  | Num _ -> Names.empty
  | Var name -> Names.singleton name
  | Add (left, right) -> Names.union (names left) (names right)
  | Let (name, rhs, body) -> Names.add name (Names.union (names rhs) (names body))
  | If (condition, yes, no) ->
    Names.union (names condition) (Names.union (names yes) (names no))
;;

let normalize source =
  let used = ref (names source) in
  let counter = ref 0 in
  let rec fresh () =
    let name = "anf" ^ string_of_int !counter in
    incr counter;
    if Names.mem name !used
    then fresh ()
    else (
      used := Names.add name !used;
      name)
  in
  let rename env name =
    match List.assoc_opt name env with
    | Some renamed -> renamed
    | None -> name
  in
  let rec go env expr k =
    let bind rhs =
      let name = fresh () in
      Bind (name, rhs, k (Id name))
    in
    match expr with
    | Num value -> k (Int value)
    | Var name -> k (Id (rename env name))
    | Add (left, right) ->
      go env left (fun left -> go env right (fun right -> bind (Plus (left, right))))
    | Let (name, rhs, body) ->
      go env rhs (fun value ->
        let renamed = fresh () in
        Bind (renamed, Value value, go ((name, renamed) :: env) body k))
    | If (condition, yes, no) ->
      go env condition (fun condition ->
        let yes = go env yes (fun value -> Return value) in
        let no = go env no (fun value -> Return value) in
        bind (Branch (condition, yes, no)))
  in
  go [] source (fun value -> Return value)
;;

let rec eval_source env = function
  | Num value -> value
  | Var name -> List.assoc name env
  | Add (left, right) ->
    let left = eval_source env left in
    let right = eval_source env right in
    left + right
  | Let (name, rhs, body) ->
    let value = eval_source env rhs in
    eval_source ((name, value) :: env) body
  | If (condition, yes, no) ->
    if eval_source env condition <> 0 then eval_source env yes else eval_source env no
;;

let eval_imm env = function
  | Int value -> value
  | Id name -> List.assoc name env
;;

let rec eval_term env = function
  | Return value -> eval_imm env value
  | Bind (name, rhs, body) ->
    let value = eval_compound env rhs in
    eval_term ((name, value) :: env) body

and eval_compound env = function
  | Value value -> eval_imm env value
  | Plus (left, right) -> eval_imm env left + eval_imm env right
  | Branch (condition, yes, no) ->
    if eval_imm env condition <> 0 then eval_term env yes else eval_term env no
;;

let rec closed_source env = function
  | Num _ -> true
  | Var name -> Names.mem name env
  | Add (left, right) -> closed_source env left && closed_source env right
  | Let (name, rhs, body) ->
    closed_source env rhs && closed_source (Names.add name env) body
  | If (condition, yes, no) ->
    closed_source env condition && closed_source env yes && closed_source env no
;;

let scoped env term =
  let seen = ref env in
  let imm env = function
    | Int _ -> true
    | Id name -> Names.mem name env
  in
  let rec check env = function
    | Return value -> imm env value
    | Bind (name, rhs, body) ->
      let rhs_ok = compound env rhs in
      let fresh = not (Names.mem name !seen) in
      seen := Names.add name !seen;
      rhs_ok && fresh && check (Names.add name env) body
  and compound env = function
    | Value value -> imm env value
    | Plus (left, right) -> imm env left && imm env right
    | Branch (condition, yes, no) -> imm env condition && check env yes && check env no
  in
  check env term
;;

let rec show = function
  | Num value -> string_of_int value
  | Var name -> name
  | Add (left, right) -> "(" ^ show left ^ " + " ^ show right ^ ")"
  | Let (name, rhs, body) -> "(let " ^ name ^ " = " ^ show rhs ^ " in " ^ show body ^ ")"
  | If (condition, yes, no) ->
    "(if " ^ show condition ^ " then " ^ show yes ^ " else " ^ show no ^ ")"
;;
