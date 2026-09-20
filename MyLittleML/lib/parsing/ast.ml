(** Abstract syntax tree of the MyLittleML front-end (Task 1).

    The language is a subset of OCaml large enough to express CPS-style
    factorial and fibonacci programs.  Multi-parameter functions keep
    their parameters as a plain string list here; desugaring into nested
    {!Fun} nodes is deliberately left to later passes (Task 2, ANF). *)

type bop =
  | Add  (** [a + b] *)
  | Sub  (** [a - b] *)
  | Mul  (** [a * b] *)
  | Div  (** [a / b] *)
  | Mod  (** [a mod b] *)
  | Lt   (** [a < b] *)
  | Le   (** [a <= b] *)
  | Gt   (** [a > b] *)
  | Ge   (** [a >= b] *)
  | Eq   (** [a = b] *)
  | Neq  (** [a <> b] *)
[@@deriving show]

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | App of expr * expr
  | Fun of string list * expr
  | Let of bind * expr option
  | If of expr * expr * expr
  | Binop of bop * expr * expr
  | Neg of expr
[@@deriving show]

and bind = { rec_ : bool; name : string; params : string list; body : expr }
[@@deriving show]

type prog = expr list [@@deriving show]