open Ast

type error = [ `parse_error of string ]

val parse_program_LL : string -> (program, error) result
