(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let rec loop n = if n = 0 then 0 else (fun x -> loop x) (n - 1);;
let () = print_endline (string_of_int (loop 4))
