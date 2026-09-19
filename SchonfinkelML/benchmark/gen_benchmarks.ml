module Gen = QCheck.Gen
open Ast_gen
open Frontend.Common

let write_generated_file ~n ~st ~dir ~gen =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  let expr = Gen.generate1 ~rand:st gen in
  let code = Format.asprintf "%a" pp_expr expr in
  let filename = Printf.sprintf "ast_expression_%d.txt" n in
  let path = Filename.concat dir filename in
  let oc = open_out path in
  output_string oc code;
  close_out oc
;;

let () =
  let st = Random.State.make [| 42 |] in
  let rec helper n bound =
    if n >= bound
    then ()
    else (
      write_generated_file ~n ~st ~dir:"./examples" ~gen:Ast_gen.gen_expr_sized;
      helper (n + 1) bound)
  in
  helper 0 100
;;
