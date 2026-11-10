[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let ( let* ) = Result.bind

let corpus blocks =
  List.init blocks (fun i ->
    Printf.sprintf
      {|let rec fact%d n k =
  if n < 1 then k 1
  else fact%d (n - 1) (fun r -> k (n * r))
let rec fib%d n k =
  if n < 2 then k n
  else fib%d (n - 1) (fun a -> fib%d (n - 2) (fun b -> k (a + b)))
let fact_result%d = fact%d 5 (fun x -> x)
let fib_result%d = fib%d 10 (fun x -> x)
|}
      i
      i
      i
      i
      i
      i
      i
      i
      i)
  |> String.concat "\n"
;;

let parse_result parse source =
  Result.map_error (fun (`Parsing_error message) -> message) (parse source)
;;

let repeats = 100

let average_seconds parse source =
  Gc.full_major ();
  let start = Sys.time () in
  let rec repeat remaining =
    if remaining = 0
    then Ok ()
    else
      let* ast = parse_result parse source in
      let (_ : MLate_frontend.Ast.program) = Sys.opaque_identity ast in
      repeat (remaining - 1)
  in
  let* () = repeat repeats in
  let elapsed = Sys.time () -. start in
  Ok (elapsed /. float_of_int repeats)
;;

let benchmark blocks =
  let source = corpus blocks in
  let ll = MLate_frontend.Topdown.parse in
  let lr = MLate_frontend.Parse.parse in
  let* ll_ast = parse_result ll source in
  let* lr_ast = parse_result lr source in
  if List.length ll_ast <> blocks * 4 || ll_ast <> lr_ast
  then Error "Parsers disagree"
  else
    let* ll_seconds = average_seconds ll source in
    let* lr_seconds = average_seconds lr source in
    Printf.printf
      "| %d | %d | %.3f | %.3f | %.2fx |\n%!"
      blocks
      (String.length source)
      (ll_seconds *. 1000.)
      (lr_seconds *. 1000.)
      (ll_seconds /. lr_seconds);
    Ok ()
;;

let () =
  print_endline "| Blocks | Bytes | Angstrom (ms) | Menhir (ms) | LL/LR |";
  print_endline "| ---: | ---: | ---: | ---: | ---: |";
  let rec run = function
    | [] -> Ok ()
    | blocks :: rest ->
      let* () = benchmark blocks in
      run rest
  in
  match run [ 1; 10; 100; 1000 ] with
  | Ok () -> ()
  | Error message ->
    prerr_endline message;
    exit 1
;;
