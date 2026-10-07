(** Independent test oracles. Source evaluation never invokes Normalize or
    traverses ANF. Both evaluators share only values and primitive semantics. *)
open Middleend

type value =
  | Scalar of Ast.constant
  | Function of (value -> value)
  | Primitive of string * value list

type failure =
  | Runtime of string
  | Fuel_exhausted

exception Stop of failure

type observation =
  { result : (Ast.constant, failure) result
  ; trace : int list
  }

type state =
  { mutable fuel : int
  ; mutable trace : int list
  }

let tick state =
  if state.fuel <= 0 then raise (Stop Fuel_exhausted);
  state.fuel <- state.fuel - 1
;;

let int = function
  | Scalar (Ast.Integer n) -> n
  | _ -> raise (Stop (Runtime "expected int"))
;;

let bool = function
  | Scalar (Ast.Boolean b) -> b
  | _ -> raise (Stop (Runtime "expected bool"))
;;

let integer n = Scalar (Ast.Integer n)
let boolean b = Scalar (Ast.Boolean b)

let lookup name env =
  match List.assoc_opt name env with
  | Some v -> v
  | None -> raise (Stop (Runtime ("unbound: " ^ name)))
;;

let primitive state name args =
  match name, args with
  | "print_int", [ argument ] ->
    state.trace <- int argument :: state.trace;
    Scalar Ast.Unit
  | "~-", [ argument ] -> integer (-int argument)
  | "~+", [ argument ] -> integer (int argument)
  | ("+" | "-" | "*" | "/" | "<" | "<=" | ">" | ">="), [ left; right ] ->
    let left = int left in
    let right = int right in
    (match name with
     | "+" -> integer (left + right)
     | "-" -> integer (left - right)
     | "*" -> integer (left * right)
     | "/" ->
       if right = 0
       then raise (Stop (Runtime "division by zero"))
       else integer (left / right)
     | "<" -> boolean (left < right)
     | "<=" -> boolean (left <= right)
     | ">" -> boolean (left > right)
     | ">=" -> boolean (left >= right)
     | _ -> assert false)
  | ("=" | "<>"), [ Scalar left; Scalar right ] ->
    boolean (if name = "=" then left = right else left <> right)
  | ("&&" | "||"), [ left; right ] ->
    let left = bool left in
    let right = bool right in
    boolean (if name = "&&" then left && right else left || right)
  | _ -> raise (Stop (Runtime ("invalid primitive: " ^ name)))
;;

let apply state fn argument =
  tick state;
  match fn with
  | Function call -> call argument
  | Scalar _ -> raise (Stop (Runtime "not a function"))
  | Primitive (name, arguments) ->
    let arguments = arguments @ [ argument ] in
    let arity = if List.mem name [ "print_int"; "~-"; "~+" ] then 1 else 2 in
    if List.length arguments = arity
    then primitive state name arguments
    else Primitive (name, arguments)
;;

let builtin_names =
  [ "+"; "-"; "*"; "/"; "="; "<>"; "<"; "<="; ">"; ">="; "&&"; "||"; "~-"; "~+" ]
;;

let external_values = [ "print_int", Primitive ("print_int", []) ]

let observe ?(fuel = 100000) action =
  let state = { fuel; trace = [] } in
  let result =
    try
      match action state with
      | Scalar value -> Ok value
      | Function _ | Primitive _ -> Error (Runtime "non-scalar result")
    with
    | Stop failure -> Error failure
  in
  { result; trace = List.rev state.trace }
;;

type binding =
  { value : value
  ; known : string option
  }

let unknown value = { value; known = None }

let source_env =
  List.map
    (fun name -> name, { value = Primitive (name, []); known = Some name })
    builtin_names
  @ List.map (fun (name, value) -> name, unknown value) external_values
;;

let bind env pattern value =
  match pattern with
  | Ast.PWildcard -> env
  | Ast.PVariable name -> (name, value) :: env
;;

let source_worker state =
  let rec eval env expression =
    tick state;
    match expression with
    | Ast.Constant constant -> unknown (Scalar constant)
    | Ast.Variable name -> lookup name env
    | Ast.Lambda (parameter, body) ->
      unknown
        (Function
           (fun argument -> (eval (bind env parameter (unknown argument)) body).value))
    | Ast.Application (Ast.Application (Ast.Variable name, left), right)
      when (lookup name env).known = Some "&&" || (lookup name env).known = Some "||" ->
      let is_and = (lookup name env).known = Some "&&" in
      let left = bool (eval env left).value in
      if (is_and && not left) || ((not is_and) && left)
      then unknown (boolean left)
      else unknown (boolean (bool (eval env right).value))
    | Ast.Application (fn, argument) ->
      let fn = (eval env fn).value in
      let argument = (eval env argument).value in
      unknown (apply state fn argument)
    | Ast.If_then_else (condition, yes, no) ->
      let condition = bool (eval env condition).value in
      unknown (eval env (if condition then yes else no)).value
    | Ast.Let_in (Ast.Nonrecursive, pattern, rhs, body) ->
      let rhs = eval env rhs in
      eval (bind env pattern rhs) body
    | Ast.Let_in (Ast.Recursive, pattern, rhs, body) ->
      let _, env = recursive env pattern rhs in
      eval env body
  and recursive env pattern rhs =
    match pattern, rhs with
    | Ast.PVariable name, Ast.Lambda (parameter, body) ->
      let cell = ref env in
      let fn =
        unknown
          (Function
             (fun argument -> (eval (bind !cell parameter (unknown argument)) body).value))
      in
      let env = (name, fn) :: env in
      cell := env;
      fn, env
    | _ -> raise (Stop (Runtime "unsupported recursion"))
  in
  let definition env (Ast.Definition (flag, pattern, rhs)) =
    match flag with
    | Ast.Nonrecursive ->
      let value = eval env rhs in
      bind env pattern value, value.value
    | Ast.Recursive ->
      let value, env = recursive env pattern rhs in
      env, value.value
  in
  eval, definition
;;

let source ?fuel expression =
  observe ?fuel (fun state ->
    let eval, _ = source_worker state in
    (eval source_env expression).value)
;;

let source_program ?fuel program =
  observe ?fuel (fun state ->
    let _, definition = source_worker state in
    let _, value =
      List.fold_left
        (fun (env, _) item -> definition env item)
        (source_env, Scalar Ast.Unit)
        program
    in
    value)
;;

let anf_worker state =
  let immediate env = function
    | Anf.Constant constant -> Scalar constant
    | Anf.Local id ->
      (match List.assoc_opt id.Anf.stamp env with
       | Some value -> value
       | None -> raise (Stop (Runtime "unbound local")))
    | Anf.External name -> lookup name external_values
    | Anf.Builtin builtin -> Primitive (Anf.builtin_name builtin, [])
  in
  let rec term env input =
    tick state;
    match input with
    | Anf.Result rhs -> compound env rhs
    | Anf.Let (id, rhs, body) ->
      let value = compound env rhs in
      term ((id.stamp, value) :: env) body
    | Anf.Let_rec (fn, parameter, body, continuation) ->
      let _, env = recursive env fn parameter body in
      term env continuation
  and compound env = function
    | Anf.Value value -> immediate env value
    | Anf.Apply (fn, argument) ->
      let fn = immediate env fn in
      let argument = immediate env argument in
      apply state fn argument
    | Anf.Unary_primitive (op, argument) ->
      primitive state (Anf.builtin_name (Anf.Unary op)) [ immediate env argument ]
    | Anf.Binary_primitive (op, left, right) ->
      let left = immediate env left in
      let right = immediate env right in
      primitive state (Anf.builtin_name (Anf.Binary op)) [ left; right ]
    | Anf.Lambda (parameter, body) ->
      Function (fun argument -> term ((parameter.stamp, argument) :: env) body)
    | Anf.If (condition, yes, no) ->
      let condition = bool (immediate env condition) in
      term env (if condition then yes else no)
  and recursive env fn parameter body =
    let cell = ref env in
    let fn_value =
      Function (fun argument -> term ((parameter.Anf.stamp, argument) :: !cell) body)
    in
    let env = (fn.Anf.stamp, fn_value) :: env in
    cell := env;
    fn_value, env
  in
  let definition env = function
    | Anf.Definition (id, rhs) ->
      let value = term env rhs in
      (id.stamp, value) :: env, value
    | Anf.Recursive_definition (fn, parameter, body) ->
      let value, env = recursive env fn parameter body in
      env, value
  in
  term, definition
;;

let anf ?fuel expression =
  observe ?fuel (fun state ->
    let term, _ = anf_worker state in
    term [] expression)
;;

let anf_program ?fuel program =
  observe ?fuel (fun state ->
    let _, definition = anf_worker state in
    let _, value =
      List.fold_left
        (fun (env, _) item -> definition env item)
        ([], Scalar Ast.Unit)
        program
    in
    value)
;;

let show_observation observation =
  let result =
    match observation.result with
    | Ok value -> Ast.show_constant value
    | Error Fuel_exhausted -> "fuel exhausted"
    | Error (Runtime error) -> error
  in
  Printf.sprintf
    "%s; trace=[%s]"
    result
    (String.concat "; " (List.map string_of_int observation.trace))
;;

let equivalent left right =
  match left.result, right.result with
  | Error Fuel_exhausted, _ | _, Error Fuel_exhausted -> false
  | _ -> left = right
;;
