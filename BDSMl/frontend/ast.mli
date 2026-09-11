type ident = string [@@deriving show { with_path = false }]

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Equal
  | NotEqual
  | More
  | Less
  | EMore
  | ELess
  | And
  | Or
[@@deriving show { with_path = false }]

type unop =
  | Neg
  | Not
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec
  | Rec
[@@deriving show { with_path = false }]

type pattern =
  | PVar of ident
  | PUnit
  | PWild
[@@deriving show { with_path = false }]

type expr =
  | Unit
  | Int of int
  | Bool of bool
  | Var of ident
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | If of expr * expr * expr
  | Let of rec_flag * pattern * expr * expr
  | Abs of pattern * expr
  | App of expr * expr
[@@deriving show { with_path = false }]

type structure_item =
  | StrLet of rec_flag * pattern * expr
  | StrEval of expr
[@@deriving show { with_path = false }]

type program = structure_item list [@@deriving show { with_path = false }]
