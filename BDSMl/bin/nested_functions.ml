(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let outer x = fun y -> fun z -> x + y + z;;
let result = outer 2 3 4;;
let () = print_endline (string_of_int result)
