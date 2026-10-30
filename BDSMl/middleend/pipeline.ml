[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type stage =
  | Parsing
  | Alpha
  | Closure_conv
  | Lambda_lift
  | Anf

let ( let* ) = Result.bind

let to_closure_conv program =
  let* program = Ast_passes.Normalize_rec.run (Ast_passes.Alpha.alpha program) in
  Ok (Ast_passes.Closure_conv.closure_conv program)
;;

let to_lambda_lift program =
  to_closure_conv program |> Result.map Ast_passes.Lambda_lift.lambda_lift
;;

let to_anf program =
  let* program = to_lambda_lift program in
  Anf.Anf_conv.anf program
;;

let run ~stop_after program =
  let open BDSMl_frontend in
  match stop_after with
  | Parsing -> Ok (Printer.print_program program)
  | Alpha -> Ok (Printer.print_program (Ast_passes.Alpha.alpha program))
  | Closure_conv -> to_closure_conv program |> Result.map Printer.print_program
  | Lambda_lift -> to_lambda_lift program |> Result.map Printer.print_program
  | Anf -> to_anf program |> Result.map Anf.Anf_pp.anf_pp
;;
