[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_lib

type imm =
  | ImmInt of int
  | ImmVar of string

type cond =
  | CondImm of imm
  | CondCmp of Ast.binop * imm * imm

type cexpr =
  | CImm of imm
  | CNeg of imm
  | CBinOp of Ast.binop * imm * imm
  | CApp of imm * imm list
  | CIf of cond * aexpr * aexpr
  | CFun of string list * aexpr

and aexpr =
  | ALet of string * cexpr * aexpr
  | ALetRec of string * string list * aexpr * aexpr
  | AExpr of cexpr

type decl =
  | DLet of string * string list * aexpr
  | DLetRec of string * string list * aexpr
  | DExpr of aexpr

type program = decl list

module Names = Set.Make (String)
module Env = Map.Make (String)

(* The state is the set of names already taken in the program, so generated
   names never clash with user names. *)
module State = struct
  type 'a t = Names.t -> 'a * Names.t

  let return x used = x, used

  let ( let* ) m f used =
    let x, used = m used in
    f x used
  ;;

  let rec first_free make n used =
    let name = make n in
    if Names.mem name used then first_free make (n + 1) used else name
  ;;

  let fresh ~start make used =
    let name = first_free make start used in
    name, Names.add name used
  ;;

  (* Keeps [name] if it is still free, otherwise renames it to [name_N].
     [_] is never used, so it is not renamed. *)
  let reserve name used =
    if String.equal name "_"
    then name, used
    else if Names.mem name used
    then fresh ~start:1 (Printf.sprintf "%s_%d" name) used
    else name, Names.add name used
  ;;

  let fresh_temp used = fresh ~start:0 (Printf.sprintf "t%d") used
  let run used m = fst (m used)
end

open State

let rec free_vars bound acc = function
  | Ast.Var x when Names.mem x bound -> acc
  | Ast.Var x -> Names.add x acc
  | Ast.Const _ -> acc
  | Ast.Neg e -> free_vars bound acc e
  | Ast.BinOp (_, a, b) | Ast.App (a, b) -> free_vars bound (free_vars bound acc a) b
  | Ast.If (c, t, e) -> free_vars bound (free_vars bound (free_vars bound acc c) t) e
  | Ast.Abs (x, body) -> free_vars (Names.add x bound) acc body
  | Ast.Let (x, e1, e2) -> free_vars (Names.add x bound) (free_vars bound acc e1) e2
  | Ast.LetRec (f, p, body, e) ->
    let bound_f = Names.add f bound in
    free_vars bound_f (free_vars (Names.add p bound_f) acc body) e
;;

(* Makes every binder unique. After this pass lets can be moved outwards and
   variables substituted without any risk of capture. *)
let rec uniquify env = function
  | Ast.Var x -> return (Ast.Var (Option.value (Env.find_opt x env) ~default:x))
  | Ast.Const n -> return (Ast.Const n)
  | Ast.Neg e ->
    let* e = uniquify env e in
    return (Ast.Neg e)
  | Ast.BinOp (op, a, b) ->
    let* a = uniquify env a in
    let* b = uniquify env b in
    return (Ast.BinOp (op, a, b))
  | Ast.App (f, a) ->
    let* f = uniquify env f in
    let* a = uniquify env a in
    return (Ast.App (f, a))
  | Ast.If (c, t, e) ->
    let* c = uniquify env c in
    let* t = uniquify env t in
    let* e = uniquify env e in
    return (Ast.If (c, t, e))
  | Ast.Abs (x, body) ->
    let* x' = reserve x in
    let* body = uniquify (Env.add x x' env) body in
    return (Ast.Abs (x', body))
  | Ast.Let (x, e1, e2) ->
    let* e1 = uniquify env e1 in
    let* x' = reserve x in
    let* e2 = uniquify (Env.add x x' env) e2 in
    return (Ast.Let (x', e1, e2))
  | Ast.LetRec (f, p, body, e) ->
    let* f' = reserve f in
    let* p' = reserve p in
    let env_f = Env.add f f' env in
    let* body = uniquify (Env.add p p' env_f) body in
    let* e = uniquify env_f e in
    return (Ast.LetRec (f', p', body, e))
;;

let rec params_of acc = function
  | Ast.Abs (x, body) -> params_of (x :: acc) body
  | body -> List.rev acc, body
;;

let rec spine args = function
  | Ast.App (f, a) -> spine (a :: args) f
  | f -> f, args
;;

(* [fun p1 ... pn -> f p1 ... pn] is just [f] when [f] is a variable other than
   the parameters: both are values, so nothing is evaluated earlier or later.
   It is checked on the normalized body, so [fun x -> f (let y = 1 in x)] is
   reduced too. *)
let eta_target params = function
  | AExpr (CApp (ImmVar f, args))
    when (not (List.mem f params))
         && List.compare_lengths params args = 0
         && List.for_all2
              (fun p a ->
                match a with
                | ImmVar x -> String.equal p x
                | ImmInt _ -> false)
              params
              args -> Some f
  | _ -> None
;;

let fold_binop op a b =
  match op, a, b with
  | Ast.Add, ImmInt x, ImmInt y -> Some (x + y)
  | Ast.Sub, ImmInt x, ImmInt y -> Some (x - y)
  | Ast.Mul, ImmInt x, ImmInt y -> Some (x * y)
  | _ -> None
;;

let is_comparison = function
  | Ast.Eq | Ast.Lt | Ast.Gt | Ast.Le | Ast.Ge -> true
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> false
;;

(* The branch taken by [if c1 < c2 then ...] is known in advance. *)
let static_branch = function
  | CondCmp (Ast.Eq, ImmInt x, ImmInt y) -> Some (x = y)
  | CondCmp (Ast.Lt, ImmInt x, ImmInt y) -> Some (x < y)
  | CondCmp (Ast.Gt, ImmInt x, ImmInt y) -> Some (x > y)
  | CondCmp (Ast.Le, ImmInt x, ImmInt y) -> Some (x <= y)
  | CondCmp (Ast.Ge, ImmInt x, ImmInt y) -> Some (x >= y)
  | CondCmp _ | CondImm _ -> None
;;

(* [norm env e k] normalizes [e] and passes the resulting complex expression to
   the continuation [k]. [env] maps variables bound to immediates, which are
   substituted instead of being bound. *)
let rec norm env e (k : cexpr -> aexpr t) =
  match e with
  | Ast.Const n -> k (CImm (ImmInt n))
  | Ast.Var x -> k (CImm (Option.value (Env.find_opt x env) ~default:(ImmVar x)))
  | Ast.Neg (Ast.Neg e) -> norm env e k
  | Ast.Neg e ->
    norm_imm env e (function
      | ImmInt n -> k (CImm (ImmInt (-n)))
      | i -> k (CNeg i))
  | Ast.BinOp (op, a, b) ->
    norm_imm env a (fun ia ->
      norm_imm env b (fun ib ->
        match fold_binop op ia ib with
        | Some n -> k (CImm (ImmInt n))
        | None -> k (CBinOp (op, ia, ib))))
  | Ast.App _ ->
    let f, args = spine [] e in
    norm_imm env f (fun f -> norm_imms env args (fun args -> k (CApp (f, args))))
  | Ast.Abs _ ->
    let params, body = params_of [] e in
    let* body = norm_tail env body in
    (match eta_target params body with
     | Some f -> k (CImm (ImmVar f))
     | None -> k (CFun (params, body)))
  | Ast.If (c, t, e) ->
    norm_cond env c (fun c ->
      match static_branch c with
      | Some taken -> norm env (if taken then t else e) k
      | None ->
        let* t = norm_tail env t in
        let* e = norm_tail env e in
        k (CIf (c, t, e)))
  | Ast.Let (x, e1, e2) ->
    norm env e1 (function
      | CImm i -> norm (Env.add x i env) e2 k
      | c ->
        let* body = norm env e2 k in
        return (ALet (x, c, body)))
  | Ast.LetRec (f, p, body, e2) ->
    let params, body = params_of [ p ] body in
    let* body = norm_tail env body in
    let* rest = norm env e2 k in
    return (ALetRec (f, params, body, rest))

and norm_imm env e k =
  norm env e (function
    | CImm i -> k i
    | c ->
      let* t = fresh_temp in
      let* body = k (ImmVar t) in
      return (ALet (t, c, body)))

(* A comparison stays in the condition instead of being bound to a name:
   RISC-V and LLVM compare and branch in one instruction. *)
and norm_cond env c k =
  match c with
  | Ast.BinOp (op, a, b) when is_comparison op ->
    norm_imm env a (fun a -> norm_imm env b (fun b -> k (CondCmp (op, a, b))))
  | c -> norm_imm env c (fun c -> k (CondImm c))

and norm_imms env es k =
  match es with
  | [] -> k []
  | e :: rest -> norm_imm env e (fun i -> norm_imms env rest (fun is -> k (i :: is)))

and norm_tail env e = norm env e (fun c -> return (AExpr c))

let fv_imm = function
  | ImmVar x -> Names.singleton x
  | ImmInt _ -> Names.empty
;;

let rec fv_cexpr = function
  | CImm i | CNeg i -> fv_imm i
  | CBinOp (_, a, b) -> Names.union (fv_imm a) (fv_imm b)
  | CApp (f, args) ->
    List.fold_left (fun acc a -> Names.union acc (fv_imm a)) (fv_imm f) args
  | CIf (c, t, e) -> Names.union (fv_cond c) (Names.union (fv_aexpr t) (fv_aexpr e))
  | CFun (params, body) -> Names.diff (fv_aexpr body) (Names.of_list params)

and fv_cond = function
  | CondImm i -> fv_imm i
  | CondCmp (_, a, b) -> Names.union (fv_imm a) (fv_imm b)

and fv_aexpr = function
  | AExpr c -> fv_cexpr c
  | ALet (x, c, body) -> Names.union (fv_cexpr c) (Names.remove x (fv_aexpr body))
  | ALetRec (f, params, fbody, body) ->
    Names.union
      (Names.diff (fv_aexpr fbody) (Names.of_list (f :: params)))
      (Names.remove f (fv_aexpr body))
;;

(* Only expressions that always terminate without errors can be dropped. *)
let is_pure = function
  | CImm _ | CNeg _ | CFun _ -> true
  | CBinOp (Ast.Div, _, _) -> false
  | CBinOp _ -> true
  | CApp _ | CIf _ -> false
;;

(* [let x = a < b in if x then ...] becomes [if a < b then ...] when [x] is not
   used anywhere else. The [if] may also be bound: [let y = if x then ...]. *)
let inline_condition x c body =
  let tested_only_once t e rest =
    not (Names.mem x (Names.union (fv_aexpr t) (Names.union (fv_aexpr e) rest)))
  in
  match c, body with
  | CBinOp (op, a, b), AExpr (CIf (CondImm (ImmVar y), t, e))
    when is_comparison op && String.equal x y && tested_only_once t e Names.empty ->
    Some (AExpr (CIf (CondCmp (op, a, b), t, e)))
  | CBinOp (op, a, b), ALet (z, CIf (CondImm (ImmVar y), t, e), rest)
    when is_comparison op && String.equal x y && tested_only_once t e (fv_aexpr rest) ->
    Some (ALet (z, CIf (CondCmp (op, a, b), t, e), rest))
  | _ -> None
;;

let rec simplify_aexpr = function
  | AExpr c -> AExpr (simplify_cexpr c)
  | ALet (x, c, body) ->
    let c = simplify_cexpr c in
    (match simplify_aexpr body with
     | AExpr (CImm (ImmVar y)) when String.equal x y -> AExpr c
     | body when is_pure c && not (Names.mem x (fv_aexpr body)) -> body
     | body -> Option.value (inline_condition x c body) ~default:(ALet (x, c, body)))
  | ALetRec (f, params, fbody, body) ->
    let body = simplify_aexpr body in
    if Names.mem f (fv_aexpr body)
    then ALetRec (f, params, simplify_aexpr fbody, body)
    else body

and simplify_cexpr = function
  | CIf (c, t, e) -> CIf (c, simplify_aexpr t, simplify_aexpr e)
  | CFun (params, body) -> CFun (params, simplify_aexpr body)
  | (CImm _ | CNeg _ | CBinOp _ | CApp _) as c -> c
;;

let imm_to_expr = function
  | ImmInt n -> Ast.Const n
  | ImmVar x -> Ast.Var x
;;

let abs params body = List.fold_right (fun p b -> Ast.Abs (p, b)) params body

let rec to_expr = function
  | AExpr c -> cexpr_to_expr c
  | ALet (x, c, body) -> Ast.Let (x, cexpr_to_expr c, to_expr body)
  | ALetRec (f, p :: params, fbody, body) ->
    Ast.LetRec (f, p, abs params (to_expr fbody), to_expr body)
  | ALetRec (f, [], fbody, body) -> Ast.Let (f, to_expr fbody, to_expr body)

and cexpr_to_expr = function
  | CImm i -> imm_to_expr i
  | CNeg i -> Ast.Neg (imm_to_expr i)
  | CBinOp (op, a, b) -> Ast.BinOp (op, imm_to_expr a, imm_to_expr b)
  | CApp (f, args) ->
    List.fold_left (fun acc a -> Ast.App (acc, imm_to_expr a)) (imm_to_expr f) args
  | CIf (c, t, e) -> Ast.If (cond_to_expr c, to_expr t, to_expr e)
  | CFun (params, body) -> abs params (to_expr body)

and cond_to_expr = function
  | CondImm i -> imm_to_expr i
  | CondCmp (op, a, b) -> Ast.BinOp (op, imm_to_expr a, imm_to_expr b)
;;

let decl_to_ast = function
  | DExpr a -> Ast.DExpr (to_expr a)
  | DLet (x, params, body) -> Ast.DLet (x, abs params (to_expr body))
  | DLetRec (f, p :: params, body) -> Ast.DLetRec (f, p, abs params (to_expr body))
  | DLetRec (f, [], body) -> Ast.DLet (f, to_expr body)
;;

let to_program program = List.map decl_to_ast program

(* [used] are the names that binders and temporaries must not take. *)
let normalize used e =
  simplify_aexpr
    (run
       used
       (let* e = uniquify Env.empty e in
        norm_tail Env.empty e))
;;

let convert e = normalize (free_vars Names.empty Names.empty e) e

(* Declarations are converted one by one. A declared name is visible to the
   rest of the program, so it is never renamed, and top-level declarations are
   never dropped. *)
let convert_decl = function
  | Ast.DExpr e -> DExpr (convert e)
  | Ast.DLet (x, e) ->
    (match normalize (Names.add x (free_vars Names.empty Names.empty e)) e with
     | AExpr (CFun (params, body)) -> DLet (x, params, body)
     | a -> DLet (x, [], a))
  | Ast.DLetRec (f, p, body) ->
    (* Not through [norm]: [let rec f x = f x] must not become [let f = f]. *)
    let fn = Ast.Abs (p, body) in
    run
      (Names.add f (free_vars Names.empty Names.empty fn))
      (let* fn = uniquify Env.empty fn in
       let params, body = params_of [] fn in
       let* body = norm_tail Env.empty body in
       return (DLetRec (f, params, simplify_aexpr body)))
;;

(* The simplifications enable each other (dropping a dead binding can make a
   function eta-reducible), so the pass is repeated until nothing changes.
   Every step only shrinks the program; the bound is just a safety net. *)
let fixpoint convert back x =
  let rec loop fuel a =
    let next = convert (back a) in
    if fuel = 0 || next = a then a else loop (fuel - 1) next
  in
  loop 16 (convert x)
;;

let of_expr e = fixpoint convert to_expr e
let of_program program = List.map (fixpoint convert_decl decl_to_ast) program

let binop_to_string = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Eq -> "="
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
;;

let pp_imm ppf = function
  | ImmInt n when n < 0 -> Format.fprintf ppf "(%d)" n
  | ImmInt n -> Format.fprintf ppf "%d" n
  | ImmVar x -> Format.pp_print_string ppf x
;;

let pp_list pp ppf xs = Format.pp_print_list ~pp_sep:Format.pp_print_space pp ppf xs
let pp_params ppf params = Format.fprintf ppf "@[<h>%a@]" (pp_list Format.pp_print_string) params

(* Expressions that are printed on one line. *)
let is_flat = function
  | CImm _ | CNeg _ | CBinOp _ | CApp _ | CFun (_, AExpr (CImm _ | CNeg _ | CBinOp _ | CApp _))
    -> true
  | CIf _ | CFun _ -> false
;;

let pp_binop ppf (op, a, b) =
  Format.fprintf ppf "%a %s %a" pp_imm a (binop_to_string op) pp_imm b
;;

let pp_cond ppf = function
  | CondImm i -> pp_imm ppf i
  | CondCmp (op, a, b) -> pp_binop ppf (op, a, b)
;;

let rec pp_cexpr ppf = function
  | CImm i -> pp_imm ppf i
  | CNeg i -> Format.fprintf ppf "-%a" pp_imm i
  | CBinOp (op, a, b) -> pp_binop ppf (op, a, b)
  | CApp (f, args) -> Format.fprintf ppf "@[<h>%a %a@]" pp_imm f (pp_list pp_imm) args
  | CIf (c, t, e) ->
    Format.fprintf ppf "@[<v>if %a@,%a@,%a@]" pp_cond c (pp_branch "then") t (pp_branch "else") e
  | CFun (params, (AExpr c as body)) when is_flat c ->
    Format.fprintf ppf "fun %a -> %a" pp_params params pp_aexpr body
  | CFun (params, body) -> Format.fprintf ppf "@[<v 2>fun %a ->@,%a@]" pp_params params pp_aexpr body

and pp_branch keyword ppf = function
  | AExpr c as a when is_flat c -> Format.fprintf ppf "%s %a" keyword pp_aexpr a
  | a -> Format.fprintf ppf "@[<v 2>%s@,%a@]" keyword pp_aexpr a

(* Compound right-hand sides are parenthesized to keep the output readable. *)
and pp_rhs ppf = function
  | (CIf _ | CFun _) as c -> Format.fprintf ppf "(%a)" pp_cexpr c
  | c -> pp_cexpr ppf c

and pp_aexpr ppf = function
  | AExpr c -> pp_cexpr ppf c
  | ALet (x, c, body) when is_flat c ->
    Format.fprintf ppf "@[<h>let %s = %a in@]@,%a" x pp_rhs c pp_aexpr body
  | ALet (x, c, body) ->
    Format.fprintf ppf "@[<v 2>let %s =@,%a@]@,in@,%a" x pp_rhs c pp_aexpr body
  | ALetRec (f, params, fbody, body) ->
    Format.fprintf
      ppf
      "@[<v 2>let rec %s %a =@,%a@]@,in@,%a"
      f
      pp_params
      params
      pp_aexpr
      fbody
      pp_aexpr
      body
;;

let pp ppf a = Format.fprintf ppf "@[<v>%a@]" pp_aexpr a

let pp_binding ppf (keyword, name, params, body) =
  let pp_head ppf () =
    match params with
    | [] -> Format.fprintf ppf "%s %s" keyword name
    | _ -> Format.fprintf ppf "%s %s %a" keyword name pp_params params
  in
  match body with
  | AExpr c when is_flat c -> Format.fprintf ppf "@[<h>%a = %a@]" pp_head () pp_cexpr c
  | _ -> Format.fprintf ppf "@[<v 2>%a =@,%a@]" pp_head () pp_aexpr body
;;

let pp_decl ppf = function
  | DExpr a -> pp_aexpr ppf a
  | DLet (x, params, body) -> pp_binding ppf ("let", x, params, body)
  | DLetRec (f, params, body) -> pp_binding ppf ("let rec", f, params, body)
;;

(* Declarations are separated by an empty line. *)
let pp_program ppf program =
  Format.fprintf
    ppf
    "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,@,") pp_decl)
    program
;;
