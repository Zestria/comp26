(** Source positions use zero-based byte offsets and one-based lines and columns. *)

type point =
  { offset : int (** Zero-based byte offset from the start of the input. *)
  ; line : int (** One-based line number. *)
  ; column : int (** One-based byte column within [line]. *)
  }
[@@deriving eq, show { with_path = false }]

(** A half-open source range: [start] is included and [finish] is excluded. *)
type span =
  { start : point
  ; finish : point
  }
[@@deriving eq, show { with_path = false }]

let of_lexing_position position =
  { offset = position.Lexing.pos_cnum
  ; line = position.Lexing.pos_lnum
  ; column = position.Lexing.pos_cnum - position.Lexing.pos_bol + 1
  }
;;

(** [of_lexbuf lexbuf] returns the range of the lexeme most recently matched
    by [lexbuf]. *)
let of_lexbuf lexbuf =
  { start = of_lexing_position (Lexing.lexeme_start_p lexbuf)
  ; finish = of_lexing_position (Lexing.lexeme_end_p lexbuf)
  }
;;

(** Print a point as [line N, column K]. *)
let pp_point formatter { line; column; _ } =
  Format.fprintf formatter "line %d, column %d" line column
;;
