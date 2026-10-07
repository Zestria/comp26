open Middleend
open Anf_test_eval

type ty =
  | Int
  | Bool
  | Unit
  | Arrow of ty * ty

type node =
  { ty : ty
  ; shape : shape
  }

and shape =
  | Literal of Ast.constant
  | Variable of string
  | Lambda of string * ty * node
  | Apply of node * node
  | Let of string * node * node
  | If of node * node * node
  | Binary of string * node * node

let node ty shape = { ty; shape }

let binary_type = function
  | "+" | "-" | "*" -> Int, Int
  | "<" | "=" -> Int, Bool
  | "&&" | "||" -> Bool, Bool
  | _ -> invalid_arg "test operator"
;;

let initial =
  List.map
    (fun name ->
       let arg, result = binary_type name in
       name, Arrow (arg, Arrow (arg, result)))
    [ "+"; "-"; "*"; "<"; "="; "&&"; "||" ]
  @ [ "print_int", Arrow (Int, Unit) ]
;;

let rec valid env node =
  match node.shape with
  | Literal (Ast.Integer _) -> node.ty = Int
  | Literal (Ast.Boolean _) -> node.ty = Bool
  | Literal Ast.Unit -> node.ty = Unit
  | Variable name -> List.assoc_opt name env = Some node.ty
  | Lambda (name, argument, body) ->
    node.ty = Arrow (argument, body.ty) && valid ((name, argument) :: env) body
  | Apply (fn, argument) ->
    fn.ty = Arrow (argument.ty, node.ty) && valid env fn && valid env argument
  | Let (name, rhs, body) ->
    node.ty = body.ty
    && valid env rhs
    && valid (if name = "_" then env else (name, rhs.ty) :: env) body
  | If (condition, yes, no) ->
    condition.ty = Bool
    && yes.ty = node.ty
    && no.ty = node.ty
    && valid env condition
    && valid env yes
    && valid env no
  | Binary (name, left, right) ->
    let argument, result = binary_type name in
    node.ty = result
    && left.ty = argument
    && right.ty = argument
    && List.assoc_opt name env = Some (Arrow (argument, Arrow (argument, result)))
    && valid env left
    && valid env right
;;

let pattern name = if name = "_" then Ast.PWildcard else Ast.PVariable name

let rec ast node =
  match node.shape with
  | Literal constant -> Ast.Constant constant
  | Variable name -> Ast.Variable name
  | Lambda (name, _, body) -> Ast.Lambda (pattern name, ast body)
  | Apply (fn, argument) -> Ast.Application (ast fn, ast argument)
  | Let (name, rhs, body) -> Ast.Let_in (Ast.Nonrecursive, pattern name, ast rhs, ast body)
  | If (condition, yes, no) -> Ast.If_then_else (ast condition, ast yes, ast no)
  | Binary (name, left, right) -> Ast.infix name (ast left) (ast right)
;;

let ( let* ) = QCheck.Gen.( >>= )
let names = QCheck.Gen.oneof_list [ "x"; "y"; "anf0" ]
let argument_types = QCheck.Gen.oneof_list [ Int; Bool; Unit; Arrow (Int, Int) ]

let rec gen_typed env ty budget =
  let open QCheck.Gen in
  let base =
    match ty with
    | Int -> map (fun n -> node Int (Literal (Ast.Integer n))) (int_range (-10) 10)
    | Bool -> map (fun b -> node Bool (Literal (Ast.Boolean b))) bool
    | Unit -> return (node Unit (Literal Ast.Unit))
    | Arrow (argument, result) ->
      let* name = names in
      let* body =
        gen_typed ((name, argument) :: env) result (Stdlib.max 0 (budget - 1))
      in
      return (node ty (Lambda (name, argument, body)))
  in
  let variables =
    List.filter_map (fun (name, t) -> if t = ty then Some name else None) env
  in
  (* Retain only the nearest binding for each name when shadowing occurs. *)
  let variables = List.filter (fun name -> List.assoc_opt name env = Some ty) variables in
  let leaf =
    match variables with
    | [] -> base
    | _ ->
      oneof [ base; map (fun name -> node ty (Variable name)) (oneof_list variables) ]
  in
  if budget = 0
  then leaf
  else (
    let child env ty = gen_typed env ty ((budget - 1) / 2) in
    let binding =
      let* name = names in
      let* value_ty = argument_types in
      let* rhs = child env value_ty in
      let* body = child ((name, value_ty) :: env) ty in
      return (node ty (Let (name, rhs, body)))
    in
    let application =
      let* argument_ty = argument_types in
      let* fn = child env (Arrow (argument_ty, ty)) in
      let* argument = child env argument_ty in
      return (node ty (Apply (fn, argument)))
    in
    let branch =
      let* condition = child env Bool in
      let* yes = gen_typed env ty ((budget - 1) / 3) in
      let* no = gen_typed env ty ((budget - 1) / 3) in
      return (node ty (If (condition, yes, no)))
    in
    let effectful =
      let* argument = child env Int in
      let rhs =
        node Unit (Apply (node (Arrow (Int, Unit)) (Variable "print_int"), argument))
      in
      let* body = child env ty in
      return (node ty (Let ("_", rhs, body)))
    in
    let operators =
      match ty with
      | Int -> [ "+"; "-"; "*" ]
      | Bool -> [ "<"; "="; "&&"; "||" ]
      | Unit | Arrow _ -> []
    in
    let operations =
      match operators with
      | [] -> []
      | _ ->
        [ ( 2
          , let* name = oneof_list operators in
            let argument, _ = binary_type name in
            let* left = child env argument in
            let* right = child env argument in
            return (node ty (Binary (name, left, right))) )
        ]
    in
    oneof_weighted
      ([ 5, leaf; 3, binding; 3, application; 2, branch; 2, effectful ] @ operations))
;;

let rec smaller node yield =
  let emit child = if child.ty = node.ty then yield child in
  let change rebuild child =
    smaller child (fun child -> yield { node with shape = rebuild child })
  in
  match node.shape with
  | Literal (Ast.Integer n) ->
    QCheck.Shrink.int n (fun n -> yield { node with shape = Literal (Ast.Integer n) })
  | Literal (Ast.Boolean true) -> yield { node with shape = Literal (Ast.Boolean false) }
  | Literal (Ast.Boolean false | Ast.Unit) | Variable _ -> ()
  | Lambda (name, argument, body) ->
    change (fun body -> Lambda (name, argument, body)) body
  | Apply (fn, argument) ->
    emit fn;
    emit argument;
    change (fun fn -> Apply (fn, argument)) fn;
    change (fun argument -> Apply (fn, argument)) argument
  | Let (name, rhs, body) ->
    emit rhs;
    emit body;
    change (fun rhs -> Let (name, rhs, body)) rhs;
    change (fun body -> Let (name, rhs, body)) body
  | If (condition, yes, no) ->
    emit yes;
    emit no;
    change (fun condition -> If (condition, yes, no)) condition;
    change (fun yes -> If (condition, yes, no)) yes;
    change (fun no -> If (condition, yes, no)) no
  | Binary (name, left, right) ->
    emit left;
    emit right;
    change (fun left -> Binary (name, left, right)) left;
    change (fun right -> Binary (name, left, right)) right
;;

let shrink node yield =
  smaller node (fun candidate -> if valid initial candidate then yield candidate)
;;

let print node = Ast.show_expr (ast node)

let arbitrary =
  QCheck.make
    ~print
    ~shrink
    QCheck.Gen.(
      sized (fun size ->
        let* ty = oneof_list [ Int; Bool; Unit ] in
        gen_typed initial ty (min 14 size)))
;;

let preservation =
  QCheck.Test.make
    ~name:"ANF preserves typed closed terms, scope, result and trace"
    ~count:1000
    arbitrary
    (fun input ->
       if not (valid initial input)
       then QCheck.Test.fail_report "generator left its domain";
       let expression = ast input in
       match Normalize.expression expression with
       | Error error ->
         QCheck.Test.fail_reportf
           "normalize: %s"
           (Format.asprintf "%a" Normalize.pp_error error)
       | Ok ir ->
         let before = source expression in
         let after = anf ir in
         if
           Check.term ~externals:[ "print_int" ] ir <> Ok ()
           || not (equivalent before after)
         then
           QCheck.Test.fail_reportf
             "source=%s\nAST=%s\nANF=%s\nIR=%s"
             (print input)
             (show_observation before)
             (show_observation after)
             (Middleend.Pretty_printer.show_term ir);
         Normalize.expression expression = Ok ir)
;;

let shrink_domain =
  QCheck.Test.make
    ~name:"ANF shrinker preserves type and scope"
    ~count:200
    arbitrary
    (fun input ->
       let okay = ref true in
       shrink input (fun candidate ->
         if candidate.ty <> input.ty || not (valid initial candidate) then okay := false);
       !okay)
;;

let () = QCheck_runner.run_tests_main [ preservation; shrink_domain ]
