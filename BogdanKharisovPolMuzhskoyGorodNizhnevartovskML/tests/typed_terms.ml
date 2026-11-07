[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Independent typed test language. Values use scalars, pairs and host
   closures, without the compiler AST or heap. *)
type _ typ =
  | Int : int typ
  | Bool : bool typ
  | Unit : unit typ
  | Pair : 'a typ * 'b typ -> ('a * 'b) typ
  | Arrow : 'a typ * 'b typ -> ('a -> 'b) typ

type (_, _) equality = Equal : ('a, 'a) equality

let rec equal_type : type a b. a typ -> b typ -> (a, b) equality option =
  fun a b ->
  match a, b with
  | Int, Int -> Some Equal
  | Bool, Bool -> Some Equal
  | Unit, Unit -> Some Equal
  | Pair (a, b), Pair (c, d) ->
    (match equal_type a c, equal_type b d with
     | Some Equal, Some Equal -> Some Equal
     | _ -> None)
  | Arrow (a, b), Arrow (c, d) ->
    (match equal_type a c, equal_type b d with
     | Some Equal, Some Equal -> Some Equal
     | _ -> None)
  | _ -> None
;;

let rec type_depth : type a. a typ -> int = function
  | Int -> 1
  | Bool -> 1
  | Unit -> 1
  | Pair (a, b) -> 1 + max (type_depth a) (type_depth b)
  | Arrow (a, b) -> 1 + max (type_depth a) (type_depth b)
;;

type 'a variable =
  { key : int list
  ; hint : string
  ; typ : 'a typ
  }

type _ expression =
  | Integer : int -> int expression
  | Boolean : bool -> bool expression
  | Unit_value : unit expression
  | Variable : 'a variable -> 'a expression
  | Tuple : 'a expression * 'b expression -> ('a * 'b) expression
  | First : ('a * 'b) expression -> 'a expression
  | Second : ('a * 'b) expression -> 'b expression
  | Function : 'a variable * 'b expression -> ('a -> 'b) expression
  | Function_two :
      'a variable * 'b variable * 'c expression
      -> ('a -> 'b -> 'c) expression
  | Apply : ('a -> 'b) expression * 'a expression -> 'b expression
  | Apply_two :
      ('a -> 'b -> 'c) expression * 'a expression * 'b expression
      -> 'c expression
  | Let : 'a variable * 'a expression * 'b expression -> 'b expression
  | If : bool expression * 'a expression * 'a expression -> 'a expression
  | Add : int expression * int expression -> int expression
  | Subtract : int expression * int expression -> int expression
  | Less : int expression * int expression -> bool expression
  | Equal_int : int expression * int expression -> bool expression
  | Strict_and : bool expression * bool expression -> bool expression
  | Not : bool expression -> bool expression

type t = Term : 'a typ * 'a expression -> t
type any_type = Type : 'a typ -> any_type
type any_variable = Binding : 'a variable -> any_variable

let rec type_of : type a. a expression -> a typ = function
  | Integer _ -> Int
  | Add _ -> Int
  | Subtract _ -> Int
  | Boolean _ -> Bool
  | Less _ -> Bool
  | Equal_int _ -> Bool
  | Strict_and _ -> Bool
  | Not _ -> Bool
  | Unit_value -> Unit
  | Variable v -> v.typ
  | Tuple (a, b) -> Pair (type_of a, type_of b)
  | First pair ->
    let (Pair (a, _)) = type_of pair in
    a
  | Second pair ->
    let (Pair (_, b)) = type_of pair in
    b
  | Function (v, body) -> Arrow (v.typ, type_of body)
  | Function_two (v, w, body) -> Arrow (v.typ, Arrow (w.typ, type_of body))
  | Apply (f, _) ->
    let (Arrow (_, result)) = type_of f in
    result
  | Apply_two (f, _, _) ->
    let (Arrow (_, Arrow (_, result))) = type_of f in
    result
  | Let (_, _, body) | If (_, body, _) -> type_of body
;;

module Keys = Set.Make (struct
    type t = int list

    let compare = compare
  end)

let rec free : type a. a expression -> Keys.t = function
  | Integer _ | Boolean _ | Unit_value -> Keys.empty
  | Variable v -> Keys.singleton v.key
  | Tuple (a, b) -> Keys.union (free a) (free b)
  | Apply (a, b) -> Keys.union (free a) (free b)
  | Add (a, b) | Subtract (a, b) | Less (a, b) | Equal_int (a, b) ->
    Keys.union (free a) (free b)
  | Strict_and (a, b) -> Keys.union (free a) (free b)
  | First p -> free p
  | Second p -> free p
  | Not e -> free e
  | Function (v, body) -> Keys.remove v.key (free body)
  | Function_two (v, w, body) -> Keys.remove v.key (Keys.remove w.key (free body))
  | Let (v, value, body) -> Keys.union (free value) (Keys.remove v.key (free body))
  | Apply_two (f, a, b) -> Keys.union (free f) (Keys.union (free a) (free b))
  | If (c, yes, no) -> Keys.union (free c) (Keys.union (free yes) (free no))
;;

let rec measure : type a. a expression -> int = function
  | Integer n -> 1 + abs n
  | Boolean true -> 2
  | Boolean false -> 1
  | Unit_value | Variable _ -> 1
  | Tuple (a, b) -> 1 + measure a + measure b
  | Apply (a, b) -> 1 + measure a + measure b
  | Add (a, b) | Subtract (a, b) | Less (a, b) | Equal_int (a, b) ->
    1 + measure a + measure b
  | Strict_and (a, b) -> 1 + measure a + measure b
  | First p -> 1 + measure p
  | Second p -> 1 + measure p
  | Not e -> 1 + measure e
  | Function (_, body) -> 1 + measure body
  | Function_two (_, _, body) -> 2 + measure body
  | Let (_, value, body) -> 1 + measure value + measure body
  | Apply_two (f, a, b) -> 1 + measure f + measure a + measure b
  | If (c, yes, no) -> 1 + measure c + measure yes + measure no
;;

let size (Term (_, e)) = measure e

let same_type (Term (a, _)) (Term (b, _)) =
  match equal_type a b with
  | Some Equal -> true
  | None -> false
;;

(* Only the printer deals with spellings. A generated binder may reuse a name.
   Substitution during shrinking can bring an older binding under that binder;
   rename precisely that conflicting binder instead of capturing the old use. *)
let binding_name names variable body =
  let conflict =
    List.exists
      (fun (key, name) -> name = variable.hint && Keys.mem key (free body))
      names
  in
  let rec fresh index =
    let name = variable.hint ^ "_fresh" ^ string_of_int index in
    if List.exists (fun (_, bound) -> bound = name) names then fresh (index + 1) else name
  in
  if conflict then fresh 0 else variable.hint
;;

let rec print : type a. (int list * string) list -> a expression -> string =
  fun names expression ->
  let local variable body = binding_name names variable body in
  let unary operator value = "(" ^ operator ^ " " ^ print names value ^ ")" in
  let binary operator a b =
    "(" ^ print names a ^ " " ^ operator ^ " " ^ print names b ^ ")"
  in
  match expression with
  | Integer n -> "(" ^ string_of_int n ^ ")"
  | Boolean b -> string_of_bool b
  | Unit_value -> "()"
  | Variable v -> List.assoc v.key names
  | Tuple (a, b) -> "(" ^ print names a ^ ", " ^ print names b ^ ")"
  | First p -> "(let (_field, _) = " ^ print names p ^ " in _field)"
  | Second p -> "(let (_, _field) = " ^ print names p ^ " in _field)"
  | Function (v, body) ->
    let name = local v body in
    "(fun " ^ name ^ " -> " ^ print ((v.key, name) :: names) body ^ ")"
  | Function_two (v, w, body) ->
    let first = local v body in
    let names = (v.key, first) :: names in
    let second = binding_name names w body in
    "(fun " ^ first ^ " " ^ second ^ " -> " ^ print ((w.key, second) :: names) body ^ ")"
  | Apply (f, a) -> "(" ^ print names f ^ " " ^ print names a ^ ")"
  | Apply_two (f, a, b) ->
    "(" ^ print names f ^ " " ^ print names a ^ " " ^ print names b ^ ")"
  | Let (v, value, body) ->
    let name = local v body in
    "(let "
    ^ name
    ^ " = "
    ^ print names value
    ^ " in "
    ^ print ((v.key, name) :: names) body
    ^ ")"
  | If (condition, yes, no) ->
    "(if "
    ^ print names condition
    ^ " then "
    ^ print names yes
    ^ " else "
    ^ print names no
    ^ ")"
  | Add (a, b) -> binary "+" a b
  | Subtract (a, b) -> binary "-" a b
  | Less (a, b) -> binary "<" a b
  | Equal_int (a, b) -> binary "=" a b
  | Strict_and (a, b) ->
    "(let _both = (&&) in _both " ^ print names a ^ " " ^ print names b ^ ")"
  | Not value -> unary "not" value
;;

let source (Term (_, expression)) = print [] expression

type resources =
  { fuel : int
  ; visited : int
  }

let mark bit resources = { resources with visited = resources.visited lor bit }

type _ value =
  | Int_value : int -> int value
  | Bool_value : bool -> bool value
  | Unit_result : unit value
  | Pair_value : 'a value * 'b value -> ('a * 'b) value
  | Closure_value :
      int * ('a value -> resources -> 'b value * resources)
      -> ('a -> 'b) value

type stored = Value : 'a variable * 'a value * bool -> stored

exception Outside_budget

let rec lookup : type a. a variable -> stored list -> a value * bool =
  fun variable -> function
  | [] -> invalid_arg "typed test term: free variable"
  | Value (bound, _, _) :: rest when bound.key <> variable.key -> lookup variable rest
  | Value (bound, value, shadowed) :: _ ->
    (match equal_type variable.typ bound.typ with
     | Some Equal -> value, shadowed
     | None -> invalid_arg "typed test term: inconsistent variable type")
;;

let bounded n =
  if n < -1_000_000 || n > 1_000_000 then raise Outside_budget;
  Int_value n
;;

(* This evaluator has no heap, recursive addresses or compiler IR. Its typed
   semantic functions are ordinary host closures. Immutable fuel is threaded
   through calls to bound higher-order duplication despite STLC termination. *)
let shadows variable environment body =
  Keys.mem variable.key (free body)
  && List.exists (fun (Value (outer, _, _)) -> outer.hint = variable.hint) environment
  && not
       (List.exists
          (fun (Value (outer, _, _)) ->
             outer.hint = variable.hint && Keys.mem outer.key (free body))
          environment)
;;

let rec evaluate : type a. resources -> stored list -> a expression -> a value * resources
  =
  fun fuel environment term ->
  if fuel.fuel <= 0 then raise Outside_budget;
  let fuel = { fuel with fuel = fuel.fuel - 1 } in
  let integer operator a b =
    let Int_value a, fuel = evaluate fuel environment a in
    let Int_value b, fuel = evaluate fuel environment b in
    operator a b, fuel
  in
  match term with
  | Integer n -> bounded n, fuel
  | Boolean b -> Bool_value b, fuel
  | Unit_value -> Unit_result, fuel
  | Variable v ->
    let value, shadowed = lookup v environment in
    value, if shadowed then mark 4 fuel else fuel
  | Tuple (a, b) ->
    let a, fuel = evaluate fuel environment a in
    let b, fuel = evaluate fuel environment b in
    Pair_value (a, b), mark 16 fuel
  | First pair ->
    let Pair_value (a, _), fuel = evaluate fuel environment pair in
    a, mark 64 fuel
  | Second pair ->
    let Pair_value (_, b), fuel = evaluate fuel environment pair in
    b, mark 64 fuel
  | Function (v, body) ->
    let shadowed = shadows v environment body in
    let captured = not (Keys.is_empty (Keys.remove v.key (free body))) in
    ( Closure_value
        ( 1
        , fun value fuel ->
            let fuel = mark 1 fuel in
            let fuel = if captured then mark 32 fuel else fuel in
            evaluate fuel (Value (v, value, shadowed) :: environment) body )
    , fuel )
  | Function_two (v, w, body) ->
    let captured =
      not (Keys.is_empty (Keys.remove v.key (Keys.remove w.key (free body))))
    in
    let call first second fuel =
      let environment = Value (v, first, shadows v environment body) :: environment in
      let shadowed = shadows w environment body in
      let fuel = mark 1 fuel in
      let fuel = if captured then mark 32 fuel else fuel in
      evaluate fuel (Value (w, second, shadowed) :: environment) body
    in
    ( Closure_value
        ( 2
        , fun first fuel ->
            Closure_value (1, fun second fuel -> call first second fuel), fuel )
    , fuel )
  | Apply (f, a) ->
    let Closure_value (arity, f), fuel = evaluate fuel environment f in
    let a, fuel = evaluate fuel environment a in
    let fuel = mark (if arity > 1 then 2 else 128) fuel in
    f a fuel
  | Apply_two (f, a, b) ->
    let Closure_value (arity, f), fuel = evaluate fuel environment f in
    let a, fuel = evaluate fuel environment a in
    let b, fuel = evaluate fuel environment b in
    let fuel = mark (if arity < 2 then 256 else 128) fuel in
    let Closure_value (remaining, f), fuel = f a fuel in
    let fuel = if remaining > 1 then mark 2 fuel else fuel in
    f b fuel
  | Let (v, value, body) ->
    let value, fuel = evaluate fuel environment value in
    evaluate fuel (Value (v, value, shadows v environment body) :: environment) body
  | If (condition, yes, no) ->
    let Bool_value condition, fuel = evaluate fuel environment condition in
    evaluate (mark 8 fuel) environment (if condition then yes else no)
  | Add (a, b) -> integer (fun a b -> bounded (a + b)) a b
  | Subtract (a, b) -> integer (fun a b -> bounded (a - b)) a b
  | Less (a, b) -> integer (fun a b -> Bool_value (a < b)) a b
  | Equal_int (a, b) -> integer (fun a b -> Bool_value (a = b)) a b
  | Strict_and (a, b) ->
    let Bool_value a, fuel = evaluate fuel environment a in
    let Bool_value b, fuel = evaluate fuel environment b in
    Bool_value (a && b), fuel
  | Not value ->
    let Bool_value value, fuel = evaluate fuel environment value in
    Bool_value (not value), fuel
;;

let profile expression = evaluate { fuel = 20_000; visited = 0 } [] expression
let run expression = fst (profile expression)

let execution_features (Term (_, expression)) =
  let _, resources = profile expression in
  List.filter_map
    (fun (bit, name) -> if resources.visited land bit <> 0 then Some name else None)
    [ 1, "function"
    ; 2, "partial"
    ; 4, "shadow"
    ; 8, "if"
    ; 16, "tuple"
    ; 32, "capture"
    ; 64, "projection"
    ; 128, "saturated"
    ; 256, "overapplication"
    ]
;;

let rec show : type a. a value -> string = function
  | Int_value n -> string_of_int n
  | Bool_value b -> string_of_bool b
  | Unit_result -> "()"
  | Pair_value (a, b) -> "(" ^ show a ^ ", " ^ show b ^ ")"
  | Closure_value _ -> "<function>"
;;

let expected (Term (_, expression)) = show (run expression)

let rec scoped : type a. any_variable list -> a expression -> bool =
  fun environment -> function
  | Integer _ | Boolean _ | Unit_value -> true
  | Variable v ->
    List.exists
      (fun (Binding other) ->
         other.key = v.key
         &&
         match equal_type v.typ other.typ with
         | Some Equal -> true
         | None -> false)
      environment
  | Tuple (a, b) -> scoped environment a && scoped environment b
  | Apply (a, b) -> scoped environment a && scoped environment b
  | Add (a, b) | Subtract (a, b) | Less (a, b) | Equal_int (a, b) ->
    scoped environment a && scoped environment b
  | Strict_and (a, b) -> scoped environment a && scoped environment b
  | First p -> scoped environment p
  | Second p -> scoped environment p
  | Not e -> scoped environment e
  | Function (v, body) -> scoped (Binding v :: environment) body
  | Function_two (v, w, body) -> scoped (Binding w :: Binding v :: environment) body
  | Let (v, value, body) ->
    scoped environment value && scoped (Binding v :: environment) body
  | Apply_two (f, a, b) ->
    scoped environment f && scoped environment a && scoped environment b
  | If (c, yes, no) ->
    scoped environment c && scoped environment yes && scoped environment no
;;

let rec deepest_type : type a. a expression -> int =
  fun expression ->
  let children =
    match expression with
    | Integer _ | Boolean _ | Unit_value | Variable _ -> 0
    | Tuple (a, b) -> max (deepest_type a) (deepest_type b)
    | Apply (a, b) -> max (deepest_type a) (deepest_type b)
    | Add (a, b) | Subtract (a, b) | Less (a, b) | Equal_int (a, b) ->
      max (deepest_type a) (deepest_type b)
    | Strict_and (a, b) -> max (deepest_type a) (deepest_type b)
    | First p -> deepest_type p
    | Second p -> deepest_type p
    | Not p -> deepest_type p
    | Function (_, body) -> deepest_type body
    | Function_two (_, _, body) -> deepest_type body
    | Let (_, value, body) -> max (deepest_type value) (deepest_type body)
    | Apply_two (f, a, b) -> max (deepest_type f) (max (deepest_type a) (deepest_type b))
    | If (c, yes, no) -> max (deepest_type c) (max (deepest_type yes) (deepest_type no))
  in
  max (type_depth (type_of expression)) children
;;

let validate (Term (_, expression)) =
  if not (scoped [] expression)
  then Error "a variable escapes its lexical scope"
  else if deepest_type expression > 4
  then Error "type depth exceeds four"
  else if measure expression > 600
  then Error "term measure exceeds 600"
  else Ok ()
;;

(* A type-preserving substitution, used for let elimination and beta reduction.
   Binding keys, not printed names, decide which variable is replaced. *)
let rec substitute : type a b. a variable -> a expression -> b expression -> b expression =
  fun variable replacement expression ->
  let sub : type c. c expression -> c expression =
    fun expression -> substitute variable replacement expression
  in
  match expression with
  | Variable v when v.key = variable.key ->
    (match equal_type v.typ variable.typ with
     | Some Equal -> replacement
     | None -> expression)
  | Variable _ | Integer _ | Boolean _ | Unit_value -> expression
  | Tuple (a, b) -> Tuple (sub a, sub b)
  | First p -> First (sub p)
  | Second p -> Second (sub p)
  | Function (v, body) ->
    if v.key = variable.key then expression else Function (v, sub body)
  | Function_two (v, w, body) ->
    if v.key = variable.key || w.key = variable.key
    then expression
    else Function_two (v, w, sub body)
  | Apply (f, a) -> Apply (sub f, sub a)
  | Apply_two (f, a, b) -> Apply_two (sub f, sub a, sub b)
  | Let (v, value, body) ->
    Let (v, sub value, if v.key = variable.key then body else sub body)
  | If (c, yes, no) -> If (sub c, sub yes, sub no)
  | Add (a, b) -> Add (sub a, sub b)
  | Subtract (a, b) -> Subtract (sub a, sub b)
  | Less (a, b) -> Less (sub a, sub b)
  | Equal_int (a, b) -> Equal_int (sub a, sub b)
  | Strict_and (a, b) -> Strict_and (sub a, sub b)
  | Not e -> Not (sub e)
;;

let rec default : type a. int list -> a typ -> a expression =
  fun path -> function
  | Int -> Integer 0
  | Bool -> Boolean false
  | Unit -> Unit_value
  | Pair (a, b) -> Tuple (default (0 :: path) a, default (1 :: path) b)
  | Arrow (argument, result) ->
    Function
      ({ key = -1 :: path; hint = "unused"; typ = argument }, default (0 :: path) result)
;;

let rec reductions : type a. a expression -> a expression list =
  fun expression ->
  let binary
    : type x y z.
      (x expression -> y expression -> z expression)
      -> x expression
      -> y expression
      -> z expression list
    =
    fun constructor a b ->
    List.map (fun a -> constructor a b) (reductions a)
    @ List.map (fun b -> constructor a b) (reductions b)
  in
  let smaller : a expression list =
    match expression with
    | Integer 0 | Boolean false | Unit_value | Variable _ -> []
    | Integer n -> [ Integer 0; Integer (n / 2) ]
    | Boolean true -> [ Boolean false ]
    | Tuple (a, b) -> binary (fun a b -> Tuple (a, b)) a b
    | First p ->
      (match p with
       | Tuple (a, _) -> [ a ]
       | _ -> [])
      @ List.map (fun p -> First p) (reductions p)
    | Second p ->
      (match p with
       | Tuple (_, b) -> [ b ]
       | _ -> [])
      @ List.map (fun p -> Second p) (reductions p)
    | Function (v, body) -> List.map (fun body -> Function (v, body)) (reductions body)
    | Function_two (v, w, body) ->
      List.map (fun body -> Function_two (v, w, body)) (reductions body)
    | Apply (f, argument) ->
      (match f with
       | Function (v, body) -> [ substitute v argument body ]
       | _ -> [])
      @ binary (fun f argument -> Apply (f, argument)) f argument
    | Apply_two (f, a, b) ->
      List.map (fun f -> Apply_two (f, a, b)) (reductions f)
      @ List.map (fun a -> Apply_two (f, a, b)) (reductions a)
      @ List.map (fun b -> Apply_two (f, a, b)) (reductions b)
    | Let (v, value, body) ->
      let remove = if Keys.mem v.key (free body) then substitute v value body else body in
      remove :: binary (fun value body -> Let (v, value, body)) value body
    | If (c, yes, no) ->
      [ yes; no ]
      @ List.map (fun c -> If (c, yes, no)) (reductions c)
      @ List.map (fun yes -> If (c, yes, no)) (reductions yes)
      @ List.map (fun no -> If (c, yes, no)) (reductions no)
    | Add (a, b) -> [ a; b ] @ binary (fun a b -> Add (a, b)) a b
    | Subtract (a, b) -> [ a; b ] @ binary (fun a b -> Subtract (a, b)) a b
    | Less (a, b) -> binary (fun a b -> Less (a, b)) a b
    | Equal_int (a, b) -> binary (fun a b -> Equal_int (a, b)) a b
    | Strict_and (a, b) -> [ a; b ] @ binary (fun a b -> Strict_and (a, b)) a b
    | Not e -> e :: List.map (fun e -> Not e) (reductions e)
  in
  default [] (type_of expression) :: smaller
;;

module Texts = Set.Make (String)

let shrink (Term (typ, expression) as original) yield =
  let consider seen candidate =
    let term = Term (typ, candidate) in
    if measure candidate >= size original || not (scoped [] candidate)
    then seen
    else (
      try
        let _ = run candidate in
        let text = source term in
        if Texts.mem text seen
        then seen
        else (
          yield term;
          Texts.add text seen)
      with
      | Outside_budget -> seen)
  in
  let _ = List.fold_left consider Texts.empty (reductions expression) in
  ()
;;

let type_choices =
  [ Type Int
  ; Type Bool
  ; Type Unit
  ; Type (Pair (Int, Bool))
  ; Type (Pair (Int, Int))
  ; Type (Arrow (Int, Int))
  ; Type (Arrow (Bool, Int))
  ; Type (Arrow (Int, Arrow (Int, Int)))
  ; Type (Arrow (Arrow (Int, Int), Int))
  ; Type (Pair (Arrow (Int, Int), Int))
  ]
;;

let choose values random = List.nth values (Random.State.int random (List.length values))

let visible variable environment =
  Binding variable
  :: List.filter (fun (Binding other) -> other.hint <> variable.hint) environment
;;

let matching : type a. a typ -> any_variable list -> a expression list =
  fun typ environment ->
  List.filter_map
    (fun (Binding v) : a expression option ->
       match equal_type typ v.typ with
       | Some Equal -> Some (Variable v)
       | None -> None)
    environment
;;

let rec generate
  : type a.
    a typ -> any_variable list -> int list -> int -> Random.State.t -> a expression
  =
  fun typ environment path budget random ->
  let binder typ = { key = path; hint = choose [ "x"; "y"; "z" ] random; typ } in
  let sub : type b. b typ -> int -> int -> b expression =
    fun typ branch budget -> generate typ environment (branch :: path) budget random
  in
  let function_value : type x y. x typ -> y typ -> int -> (x -> y) expression =
    fun argument result remaining ->
    match result with
    | Arrow (second, result) when Random.State.bool random ->
      let first = { (binder argument) with key = 0 :: path } in
      let second = { (binder second) with key = 1 :: path } in
      let inner = visible second (visible first environment) in
      Function_two (first, second, generate result inner (2 :: path) remaining random)
    | _ ->
      let v = binder argument in
      Function (v, generate result (visible v environment) (0 :: path) remaining random)
  in
  let base () =
    let variables = matching typ environment in
    if variables <> [] && Random.State.int random 4 <> 0
    then choose variables random
    else (
      match typ with
      | Int -> Integer (Random.State.int random 17 - 8)
      | Bool -> Boolean (Random.State.bool random)
      | Unit -> Unit_value
      | Pair (a, b) -> Tuple (sub a 0 0, sub b 1 0)
      | Arrow (argument, result) -> function_value argument result 0)
  in
  if budget <= 0
  then base ()
  else (
    let half = (budget - 1) / 2 in
    let third = (budget - 1) / 3 in
    let choice = Random.State.int random 12 in
    let choice =
      if
        (type_depth typ >= 4 && List.mem choice [ 4; 5; 6; 7 ])
        || (type_depth typ >= 3 && choice = 6)
      then 8
      else choice
    in
    match choice with
    | 0 -> base ()
    | 1 | 2 ->
      let (Type bound_type) = choose type_choices random in
      let v = binder bound_type in
      let value = sub bound_type 0 half in
      let body = generate typ (visible v environment) (1 :: path) half random in
      Let (v, value, body)
    | 3 -> If (sub Bool 0 third, sub typ 1 third, sub typ 2 third)
    | 4 | 5 ->
      let (Type argument) = choose type_choices random in
      Apply (sub (Arrow (argument, typ)) 0 half, sub argument 1 half)
    | 6 ->
      let (Type a) = choose type_choices random in
      let short_types =
        List.filter (fun (Type typ) -> type_depth typ <= 2) type_choices
      in
      let (Type b) = choose short_types random in
      Apply_two (sub (Arrow (a, Arrow (b, typ))) 0 third, sub a 1 third, sub b 2 third)
    | 7 ->
      let (Type other) = choose type_choices random in
      if Random.State.bool random
      then First (sub (Pair (typ, other)) 0 (budget - 1))
      else Second (sub (Pair (other, typ)) 0 (budget - 1))
    | _ ->
      (match typ with
       | Int ->
         if Random.State.bool random
         then Add (sub Int 0 half, sub Int 1 half)
         else Subtract (sub Int 0 half, sub Int 1 half)
       | Bool ->
         (match Random.State.int random 4 with
          | 0 -> Less (sub Int 0 half, sub Int 1 half)
          | 1 -> Equal_int (sub Int 0 half, sub Int 1 half)
          | 2 -> Strict_and (sub Bool 0 half, sub Bool 1 half)
          | _ -> Not (sub Bool 0 (budget - 1)))
       | Unit -> base ()
       | Pair (a, b) -> Tuple (sub a 0 half, sub b 1 half)
       | Arrow (argument, result) -> function_value argument result (budget - 1)))
;;

let gen_with_choices choices random =
  let rec attempt remaining =
    let (Type typ) = choose choices random in
    let expression = generate typ [] [] (12 + Random.State.int random 33) random in
    try
      if measure expression > 600 then raise Outside_budget;
      let _ = run expression in
      Term (typ, expression)
    with
    | Outside_budget ->
      if remaining = 0 then Term (Int, Integer 0) else attempt (remaining - 1)
  in
  attempt 30
;;

let gen =
  gen_with_choices
    [ Type Int; Type Bool; Type (Pair (Int, Bool)); Type (Pair (Int, Int)) ]
;;

let make_arbitrary gen =
  QCheck.make
    ~print:(fun term -> source term ^ "\nOracle: " ^ expected term)
    ~small:size
    ~shrink
    ~collect:(fun term ->
      let live = execution_features term in
      if
        List.for_all
          (fun feature -> List.mem feature live)
          [ "function"; "partial"; "shadow"; "if"; "tuple" ]
      then "all five features executed"
      else if List.length live >= 2
      then "multiple features executed"
      else "zero or one feature executed")
    gen
;;

let arbitrary = make_arbitrary gen
let arbitrary_int = make_arbitrary (gen_with_choices [ Type Int ])

let shrink_witness () =
  let x = { key = [ 0 ]; hint = "x"; typ = Int } in
  let y = { key = [ 1 ]; hint = "x"; typ = Int } in
  let f = { key = [ 2 ]; hint = "f"; typ = Arrow (Int, Int) } in
  let body =
    Let
      ( x
      , Integer 7
      , Let
          ( f
          , Function (y, Subtract (Variable x, Variable y))
          , If
              ( Boolean true
              , Apply (Variable f, First (Tuple (Integer 5, Boolean false)))
              , Integer 0 ) ) )
  in
  Term (Int, body)
;;
