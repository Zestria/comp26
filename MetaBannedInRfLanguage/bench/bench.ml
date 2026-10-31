[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let fac_cps =
  {|let rec fac_cps n k =
  if n <= 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))
;;

let main = fac_cps 5 (fun x -> x)
;;
|}
;;

let fib_cps =
  {|let rec fib_cps n k =
  match n with
  | 0 -> k 0
  | 1 -> k 1
  | n -> fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
;;

let main = fib_cps 10 (fun x -> x)
;;
|}
;;

let repeat times source = String.concat "" (List.init times (fun _ -> source))

(* [1 + 2 * 3 - 4 + ...]: stresses the operator precedence levels. *)
let long_infix length =
  let operators = [| "+"; "*"; "-"; "/"; "<"; "&&"; "||"; "=" |] in
  let buffer = Buffer.create (length * 4) in
  Buffer.add_string buffer "let x = 0";
  for i = 1 to length do
    Buffer.add_string
      buffer
      (Printf.sprintf " %s %d" operators.(i mod Array.length operators) i)
  done;
  Buffer.add_string buffer "\n;;\n";
  Buffer.contents buffer
;;

(* [let x = ((((... 1 ...))))]: stresses nesting. *)
let deep_parens depth =
  Printf.sprintf "let x = %s1%s\n;;\n" (String.make depth '(') (String.make depth ')')
;;

let inputs =
  [ "fac_cps", fac_cps
  ; "fib_cps", fib_cps
  ; "fac+fib x10", repeat 10 (fac_cps ^ fib_cps)
  ; "fac+fib x100", repeat 100 (fac_cps ^ fib_cps)
  ; "fac+fib x1000", repeat 1000 (fac_cps ^ fib_cps)
  ; "infix x1000", long_infix 1000
  ; "parens x500", deep_parens 500
  ]
;;

let parsers =
  [ ("topdown", fun source -> Rd_parser.parse_structure source)
  ; "menhir", Menhir_parser.parse_structure
  ]
;;

(* Both parsers must succeed and agree, otherwise the comparison is meaningless. *)
let check_input (name, source) =
  match List.map (fun (_, parse) -> parse source) parsers with
  | [ Ok topdown; Ok menhir ] when topdown = menhir -> Ok ()
  | [ Ok _; Ok _ ] -> Error (Printf.sprintf "%s: parsers produce different ASTs" name)
  | _ -> Error (Printf.sprintf "%s: one of the parsers fails" name)
;;

let fail message =
  prerr_endline ("bench: " ^ message);
  exit 2
;;

let bench_input seconds (name, source) =
  Printf.printf "\n=== %s (%d bytes) ===\n%!" name (String.length source);
  let samples =
    Benchmark.throughputN
      ~style:Benchmark.Nil
      ~repeat:3
      seconds
      (List.map (fun (parser_name, parse) -> parser_name, parse, source) parsers)
  in
  Benchmark.tabulate samples
;;

let () =
  let seconds =
    match Sys.argv with
    | [| _ |] -> Some 1
    | [| _; seconds |] ->
      (match int_of_string_opt seconds with
       | Some seconds when seconds > 0 -> Some seconds
       | _ -> None)
    | _ -> None
  in
  match seconds with
  | None -> fail "usage: bench.exe [seconds per measurement]"
  | Some seconds ->
    List.iter
      (fun input ->
         match check_input input with
         | Ok () -> ()
         | Error message -> fail message)
      inputs;
    List.iter (bench_input seconds) inputs
;;
