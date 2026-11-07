(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let x = 5;;
let result =
  let x = x + 1 in
  let x = x + 1 in
  (fun x -> x + 1) x
;;

let () = print_endline (string_of_int result)
