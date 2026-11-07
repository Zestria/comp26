[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** ANF conversion for the MyLittleML middle-end (Task 2).

    Atoms are constants and variables; every intermediate compound expression
    is bound by an explicit [ALet] whose right-hand side is deliberately
    [aexp], not [cexp]: the ANF generator itself only ever puts cexp-shaped
    nodes there (see {!is_cexp_rhs}), while later passes (closure conversion)
    reuse the same type with richer right-hand sides. *)

module A = Ast
module StringSet = Names.StringSet

type rec_flag =
  | Rec
  | Nonrec
[@@deriving show]

type atom =
  | AInt of int
  | ABool of bool
  | AVar of string
[@@deriving show]

type cexp =
  | CAtom of atom
  | CBinop of A.bop * atom * atom
  | CNeg of atom
  | CApp of atom * atom
  | CIf of atom * aexp * aexp
  | CFun of string list * aexp
[@@deriving show]

and aexp =
  | ACexp of cexp
  | ALet of rec_flag * string * aexp * aexp
[@@deriving show]

type item =
  | ADecl of rec_flag * string * aexp
  | AExp of aexp
[@@deriving show]

type aprogram = item list [@@deriving show]

(** Right-hand sides produced by the ANF generator are always cexp-shaped:
    either a bare [ACexp] or a chain of [ALet]s ending in one.  Closure
    conversion later nests lets in rhs positions, which this predicate
    rejects — [test_mid] uses it as the ANF flatness invariant. *)
let rec is_cexp_rhs : aexp -> bool = function
  | ACexp _ -> true
  | ALet (_, _, _, rest) -> is_cexp_rhs rest
;;

(** All identifiers occurring in a source program, used to seed fresh-name
    generation so that [anf_t<i>] temporaries never collide with user names. *)
let collect_names (prog : A.prog) : Names.StringSet.t =
  let add_vars acc params =
    List.fold_left (fun s v -> Names.StringSet.add v s) acc params
  in
  let rec expr acc = function
    | A.Int _ | A.Bool _ -> acc
    | A.Var v -> Names.StringSet.add v acc
    | A.App (f, x) -> expr (expr acc f) x
    | A.Fun (params, body) -> expr (add_vars acc params) body
    | A.Let (b, cont) ->
      let acc = add_vars (Names.StringSet.add b.A.name acc) b.A.params in
      let acc = expr acc b.A.body in
      (match cont with
       | None -> acc
       | Some e -> expr acc e)
    | A.If (c, t, f) -> expr (expr (expr acc c) t) f
    | A.Binop (_, l, r) -> expr (expr acc l) r
    | A.Neg e -> expr acc e
  in
  List.fold_left expr Names.StringSet.empty prog
;;

open State

let rec_flag_of_bool rec_ = if rec_ then Rec else Nonrec

let bind_temp c k =
  let* name = Names.fresh "anf_t" in
  let* rest = k (AVar name) in
  return
    (match rest with
     | ACexp (CAtom (AVar result)) when result = name -> ACexp c
     | _ -> ALet (Nonrec, name, ACexp c, rest))
;;

let rec norm e k =
  match e with
  | A.Int n -> k (AInt n)
  | A.Bool b -> k (ABool b)
  | A.Var v -> k (AVar v)
  | A.Let (b, cont) -> norm_let b cont k
  | _ -> norm_compound e (fun c -> bind_temp c k)

and norm_compound e k =
  match e with
  | A.Neg e -> norm e (fun a -> k (CNeg a))
  | A.Binop (op, l, r) -> norm l (fun al -> norm r (fun ar -> k (CBinop (op, al, ar))))
  | A.App (f, x) -> norm f (fun af -> norm x (fun ax -> k (CApp (af, ax))))
  | A.If (c, t, f) ->
    norm c (fun ac ->
      let* t = norm_tail t in
      let* f = norm_tail f in
      k (CIf (ac, t, f)))
  | A.Fun (params, body) ->
    let* body = norm_tail body in
    k (CFun (params, body))
  | A.Int n -> k (CAtom (AInt n))
  | A.Bool b -> k (CAtom (ABool b))
  | A.Var v -> k (CAtom (AVar v))
  | A.Let (b, cont) -> norm_let b cont (fun a -> k (CAtom a))

and norm_let b cont k =
  let* rhs = norm_binding b in
  let* rest =
    match cont with
    | None -> k (AVar b.A.name)
    | Some body -> norm body k
  in
  return (ALet (rec_flag_of_bool b.A.rec_, b.A.name, rhs, rest))

and norm_binding b =
  let* body = norm_tail b.A.body in
  return
    (match b.A.params with
     | [] -> body
     | params -> ACexp (CFun (params, body)))

and norm_tail = function
  | A.Let (b, cont) -> norm_let b cont (fun a -> return (ACexp (CAtom a)))
  | e -> norm_compound e (fun c -> return (ACexp c))
;;

let anf_prog prog =
  let item = function
    | A.Let (b, None) ->
      let* rhs = norm_binding b in
      return (ADecl (rec_flag_of_bool b.A.rec_, b.A.name, rhs))
    | e ->
      let* e = norm_tail e in
      return (AExp e)
  in
  State.eval (State.map_list item prog) (Names.make (collect_names prog))
;;

(* ---------------------------------------------------------------------- *)
(** Pretty-printing for ANF programs. *)

let bop_to_string = function
  | A.Add -> "+"
  | A.Sub -> "-"
  | A.Mul -> "*"
  | A.Div -> "/"
  | A.Mod -> "mod"
  | A.Lt -> "<"
  | A.Le -> "<="
  | A.Gt -> ">"
  | A.Ge -> ">="
  | A.Eq -> "="
  | A.Neq -> "<>"
;;

let rec pp_atom ppf = function
  | AInt n -> Format.fprintf ppf "%d" n
  | ABool b -> Format.fprintf ppf "%b" b
  | AVar v -> Format.fprintf ppf "%s" v

and pp_cexp ppf = function
  | CAtom a -> pp_atom ppf a
  | CBinop (op, l, r) ->
    Format.fprintf ppf "(%a %s %a)" pp_atom l (bop_to_string op) pp_atom r
  | CNeg a -> Format.fprintf ppf "(-%a)" pp_atom a
  | CApp (f, x) -> Format.fprintf ppf "(%a %a)" pp_atom f pp_atom x
  | CIf (c, t, f) ->
    Format.fprintf ppf "if %a then %a else %a" pp_atom c pp_aexp t pp_aexp f
  | CFun (params, body) ->
    Format.fprintf ppf "fun %s -> %a" (String.concat " " params) pp_aexp body

and pp_aexp ppf = function
  | ACexp c -> pp_cexp ppf c
  | ALet (flag, name, rhs, rest) ->
    Format.fprintf
      ppf
      "let %s%s = %a in %a"
      (if flag = Rec then "rec " else "")
      name
      pp_aexp
      rhs
      pp_aexp
      rest
;;

let pp_item ppf = function
  | ADecl (flag, name, rhs) ->
    Format.fprintf
      ppf
      "let %s%s = %a"
      (if flag = Rec then "rec " else "")
      name
      pp_aexp
      rhs
  | AExp e -> pp_aexp ppf e
;;

let pp_aprogram ppf prog =
  List.iter (fun item -> Format.fprintf ppf "%a;;\n" pp_item item) prog
;;

let to_string = Format.asprintf "%a" pp_aprogram
