open Benchmark
open Top_down

let top_down_parser =
  fun s ->
  match Top_down.Parser.parse_expr s with
  | Ok v -> v
  | Error e ->
    Format.printf "Error: %a\n" Top_down.Parser.pp_error e;
    assert false
;;

let bottom_up_parser =
  fun s -> Frontend.Bottom_up_parser.program Frontend.Lexer.token (Lexing.from_string s)
;;

let read_file p =
  let ic = open_in p in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let files = (fun i -> Printf.sprintf "examples/ast_expression_%d.txt" i) |> List.init 100

let () =
  let t = 1 in
  let repeat = 2 in
  let samples =
    List.concat_map
      (fun path ->
         let content = read_file path in
         throughputN
           ~repeat
           t
           [ "top_down_parser", (fun s -> ignore (top_down_parser s)), content
           ; "bottom_up_parser", (fun s -> ignore (bottom_up_parser s)), content
           ])
      files
  in
  tabulate samples
;;
