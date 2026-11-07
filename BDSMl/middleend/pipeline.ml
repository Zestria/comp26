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

let run ~stop_after program =
  let open BDSMl_frontend in
  let ( let* ) = Result.bind in
  match stop_after with
  | Parsing -> Ok (Printer.print_program program)
  | Alpha -> Ok (Printer.print_program (Ast_passes.Alpha.alpha program))
  | Closure_conv | Lambda_lift | Anf as stage ->
    let* program = Ast_passes.Normalize_rec.run (Ast_passes.Alpha.alpha program) in
    let program = Ast_passes.Closure_conv.closure_conv program in
    let finish = function
      | Closure_conv -> Ok (Printer.print_program program)
      | Lambda_lift ->
        Ok (Printer.print_program (Ast_passes.Lambda_lift.lambda_lift program))
      | Anf ->
        Ast_passes.Lambda_lift.lambda_lift program
        |> Anf.Anf_conv.anf
        |> Result.map Anf.Anf_pp.anf_pp
      | Parsing | Alpha -> Error "Internal error: unexpected compilation stage"
    in
    finish stage
;;
