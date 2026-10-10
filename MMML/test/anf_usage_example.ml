open Middleend

let () =
  let source = "fun c -> h (if c then f 1 else g 2)" in
  let ast =
    match Parser_menhir.parse_expression source with
    | Ok ast -> ast
    | Error error -> failwith (Format.asprintf "%a" Error_monad.pp_error error)
  in
  let ir =
    match Normalize.expression ast with
    | Ok ir -> ir
    | Error error -> failwith (Format.asprintf "%a" Normalize.pp_error error)
  in
  match Check.term ~externals:[ "h"; "f"; "g" ] ir with
  | Ok () -> Format.printf "%a@." Pretty_printer.pp_term ir
  | Error error -> failwith (Format.asprintf "%a" Check.pp_error error)
;;
