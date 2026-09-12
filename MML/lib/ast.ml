type identifier = string [@@deriving eq, show]

type constant =
  | Integer of int
  | Boolean of bool
  | Unit
[@@deriving eq, show]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving eq, show]

type pattern =
  | PWildcard
  | PVariable of identifier
  | PConstant of constant
  | PList of pattern list
  | PCons of pattern * pattern
  | PTuple of pattern * pattern * pattern list
[@@deriving eq, show]

type expr =
  | Constant of constant
  | Variable of identifier
  | Tuple of expr * expr * expr list
  | List of expr list
  | Lambda of pattern * expr
  | Application of expr * expr
  | If_then_else of expr * expr * expr
  | Let_in of rec_flag * value_binding * value_binding list * expr
  | Function of case * case list
  | Match of expr * case * case list

and value_binding =
  { pattern : pattern
  ; expression : expr
  }

and case =
  { case_pattern : pattern
  ; case_expression : expr
  }
[@@deriving eq, show]

type structure_item =
  | Value of rec_flag * value_binding * value_binding list
[@@deriving eq, show]

type program = structure_item list [@@deriving eq, show]

let application = List.fold_left (fun function_ argument -> Application (function_, argument))

let function_ parameters body =
  List.fold_right (fun parameter body -> Lambda (parameter, body)) parameters body

let infix operator left right = application (Variable operator) [ left; right ]
let prefix operator operand = Application (Variable operator, operand)
