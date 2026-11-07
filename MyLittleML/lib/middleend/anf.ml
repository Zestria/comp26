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
open Names
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

(** Fresh-name state local to one {!anf_prog} call: [anf_t0], [anf_t1], …
    skipping anything already taken by {!collect_names}.  State is now
    shared with closure conversion and lambda lifting through {!Names}. *)
type fresh = Names.t

let rec_flag_of_bool (rec_ : bool) : rec_flag = if rec_ then Rec else Nonrec

(** Smart constructor for [ALet]: [let x = v in x] is just [v].  Only non-recursive
    bindings fuse: under [Rec] the name may occur in its own right-hand
    side, so the binding must stay.  Applied to every [ALet] the ANF
    generator builds. *)
let mk_alet (flag : rec_flag) (name : string) (rhs : aexp) (rest : aexp) : aexp =
  match rhs, rest with
  | ACexp (CAtom (AVar v)), _ when v = name && flag = Nonrec -> rest
  | _ -> ALet (flag, name, rhs, rest)
;;

(** Bind a compound [cexp] to a fresh temporary and pass the atom to [k]. *)
let bind_temp (st : fresh) (c : cexp) (k : atom -> aexp) : aexp =
  let t = Names.fresh st "anf_t" in
  ALet (Nonrec, t, ACexp c, k (AVar t))
;;

let rec norm_named (st : fresh) (name : string) (rhs : A.expr) (cont : aexp) : aexp =
  let rhs' = norm_tail st rhs in
  mk_alet (rec_flag_of_bool false) name rhs' cont

and norm (st : fresh) (e : A.expr) (k : atom -> aexp) : aexp =
  match e with
  | A.Int n -> k (AInt n)
  | A.Bool b -> k (ABool b)
  | A.Var v -> k (AVar v)
  | A.Neg e -> norm st e (fun a -> bind_temp st (CNeg a) k)
  | A.Binop (op, l, r) ->
    norm st l (fun al -> norm st r (fun ar -> bind_temp st (CBinop (op, al, ar)) k))
  | A.App (f, x) ->
    norm st f (fun af -> norm st x (fun ax -> bind_temp st (CApp (af, ax)) k))
  | A.If (c, t, f) ->
    norm st c (fun ac -> bind_temp st (CIf (ac, norm_tail st t, norm_tail st f)) k)
  | A.Fun (params, body) -> bind_temp st (CFun (params, norm_tail st body)) k
  | A.Let (b, Some (A.Var v)) when (not b.A.rec_) && b.A.params = [] && v = b.A.name ->
    norm_tail st b.A.body
  | A.Let (b, cont) ->
    let flag = rec_flag_of_bool b.A.rec_ in
    (match b.A.params, cont with
     | [], Some body -> norm_named st b.A.name b.A.body (norm_tail st body)
     | [], None -> norm_named st b.A.name b.A.body (ACexp (CAtom (AVar b.A.name)))
     | _ :: _, Some body ->
       mk_alet
         flag
         b.A.name
         (ACexp (CFun (b.A.params, norm_tail st b.A.body)))
         (norm_tail st body)
     | _ :: _, None ->
       mk_alet
         flag
         b.A.name
         (ACexp (CFun (b.A.params, norm_tail st b.A.body)))
         (ACexp (CAtom (AVar b.A.name))))

and norm_tail (st : fresh) (e : A.expr) : aexp =
  match e with
  | A.Int n -> ACexp (CAtom (AInt n))
  | A.Bool b -> ACexp (CAtom (ABool b))
  | A.Var v -> ACexp (CAtom (AVar v))
  | A.Neg e -> norm st e (fun a -> ACexp (CNeg a))
  | A.Binop (op, l, r) ->
    norm st l (fun al -> norm st r (fun ar -> ACexp (CBinop (op, al, ar))))
  | A.App (f, x) -> norm st f (fun af -> norm st x (fun ax -> ACexp (CApp (af, ax))))
  | A.If (c, t, f) ->
    norm st c (fun ac -> ACexp (CIf (ac, norm_tail st t, norm_tail st f)))
  | A.Fun ([], body) -> norm_tail st body
  | A.Fun (params, body) -> ACexp (CFun (params, norm_tail st body))
  | A.Let (b, Some (A.Var v)) when (not b.A.rec_) && b.A.params = [] && v = b.A.name ->
    norm_tail st b.A.body
  | A.Let (b, cont) ->
    let flag = rec_flag_of_bool b.A.rec_ in
    (match b.A.params, cont with
     | [], Some body -> norm_named st b.A.name b.A.body (norm_tail st body)
     | [], None -> norm_named st b.A.name b.A.body (ACexp (CAtom (AVar b.A.name)))
     | _ :: _, Some body ->
       mk_alet
         flag
         b.A.name
         (ACexp (CFun (b.A.params, norm_tail st b.A.body)))
         (norm_tail st body)
     | _ :: _, None ->
       mk_alet
         flag
         b.A.name
         (ACexp (CFun (b.A.params, norm_tail st b.A.body)))
         (ACexp (CAtom (AVar b.A.name))))
;;

(** Convert a whole source program: declaration phrases become [ADecl] items,
    every other phrase becomes a bare [AExp]. *)
let anf_prog (prog : A.prog) : aprogram =
  let st = Names.make (collect_names prog) in
  let anf_item = function
    | A.Let (b, None) ->
      let flag = rec_flag_of_bool b.A.rec_ in
      (match b.A.params with
       | [] -> ADecl (flag, b.A.name, norm_tail st b.A.body)
       | params -> ADecl (flag, b.A.name, ACexp (CFun (params, norm_tail st b.A.body))))
    | e -> AExp (norm_tail st e)
  in
  List.map anf_item prog
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
