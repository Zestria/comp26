(* SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse_expression : string -> (Ast.expression, string) result
val parse_program : string -> (Ast.program, string) result
