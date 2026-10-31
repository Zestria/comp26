open Shmel_lib
open Shmel_middleend

let write_file path text =
  Out_channel.with_open_text path (fun oc -> output_string oc text)
;;

let read_file path = In_channel.with_open_text path In_channel.input_all

let cases =
  [ "empty", ""
  ; "silent_result", "let x = 42\nlet main = x + 1"
  ; "zero", "print_int 0"
  ; "negative", "print_int (-42)"
  ; "large", "print_int (-1099511627776)"
  ; "global", "let x = 20\nlet y = x + 22\nlet main = print_int y"
  ; "effects", "let a = print_int 1\nlet b = print_int 2\nlet main = print_int 3"
  ; ( "global_shadowing"
    , "let x = 10\nlet f y = x + y\nlet x = 100\nlet main = print_int (f 1)" )
  ; "previous_function", "let f x = x + 1\nlet f x = f x + 10\nlet main = print_int (f 2)"
  ; ( "builtin_shadowing"
    , "let p = print_int\nlet print_int x = x + 1\nlet main = p (print_int 41)" )
  ; "builtin_capture", "let f x = print_int x\nlet print_int x = x + 1\nlet main = f 42"
  ; "print_value", "let run f x = f x in let p = print_int in run p 42"
  ; "partial", "let f a b c = a * 100 + b * 10 + c in let g = f 1 in print_int (g 2 3)"
  ; ( "overapplication"
    , "let f x = if x = 0 then (fun y -> y + 1) else (fun y -> y + 2) in print_int (f 0 \
       7)" )
  ; ( "independent_closures"
    , "let make x = fun y -> x + y in let f = make 10 in let g = make 20 in print_int (f \
       1 + g 2)" )
  ; ( "recursive_capture"
    , "let make x = let rec f n = if n = 0 then x else f (n - 1) + 1 in f in let g = \
       make 3 in print_int (g 7)" )
  ; ( "argument_effects"
    , "let emit x = let _ = print_int x in x in let add a b = a + b in print_int (add \
       (emit 1) (emit 2))" )
  ; ( "partial_effects"
    , "let f x y = let _ = print_int x in y in let g = f 3 in let _ = print_int 1 in \
       print_int (g 7)" )
  ; ( "eight_arguments"
    , "let f a b c d e f g h = a+b+c+d+e+f+g+h in print_int (f 1 2 3 4 5 6 7 8)" )
  ; ( "eight_arguments_with_capture"
    , "let bump x = x + 1 in let f a b c d e f g = let x = bump a in x+b+c+d+e+f+g in \
       print_int (f 1 2 3 4 5 6 7)" )
  ; "nine_arguments", "let f a b c d e f g h i = a + i in print_int (f 1 2 3 4 5 6 7 8 9)"
  ; ( "ten_arguments_with_capture"
    , "let bump x = x + 1 in let f a b c d e f g h i j = let x = bump a in \
       x+b+c+d+e+f+g+h+i+j in print_int (f 1 2 3 4 5 6 7 8 9 10)" )
  ; ( "reserved_symbols"
    , "let shmel_print_int x = x + 1\n\
       let shmel_fn_0 x = shmel_print_int x\n\
       let main = print_int (shmel_fn_0 41)" )
  ]
;;

let expected name source =
  let program =
    match Main.parse_program_with_comb source with
    | Ok program -> program
    | Error (`Parsing_error message) -> failwith (name ^ ": " ^ message)
  in
  let outcome = Eval.run program in
  match outcome.result with
  | Ok _ -> outcome.output
  | Error error -> failwith (Format.asprintf "%s: %a" name Eval.pp_error error)
;;

let run ~docker =
  let cases =
    cases @ [ "fac_cps", read_file "fac_cps.ml"; "fib_cps", read_file "fib_cps.ml" ]
  in
  if not (Sys.file_exists "programs") then Sys.mkdir "programs" 0o700;
  let manifest =
    List.concat_map
      (fun (name, source) ->
         let output = expected name source in
         let input = "programs/" ^ name ^ ".ml" in
         write_file input source;
         List.map
           (fun parser ->
              let name = name ^ "_" ^ parser in
              let base = "programs/" ^ name in
              write_file (base ^ ".expected") output;
              let command =
                String.concat
                  " "
                  (List.map
                     Filename.quote
                     [ "../../compiler.exe"
                     ; "-parsing"
                     ; parser
                     ; "-o"
                     ; base ^ ".s"
                     ; input
                     ])
              in
              if Sys.command command <> 0 then failwith ("compiler failed: " ^ name);
              name)
           [ "topdown"; "menhir" ])
      cases
  in
  write_file "programs/manifest" (String.concat "\n" manifest ^ "\n");
  let available = Sys.command "sh run_riscv.sh available >/dev/null 2>&1" = 0 in
  if (not docker) && not available
  then
    Printf.printf
      "%d whole-program cases compiled; skipped QEMU: cross-compiler or QEMU not found\n"
      (List.length manifest)
  else (
    let command =
      if docker
      then "sh docker_runner.sh programs"
      else "sh run_riscv.sh programs ../../backend"
    in
    if Sys.command command <> 0 then failwith ("program tests failed: " ^ command))
;;

let () =
  match Array.to_list Sys.argv with
  | [ _ ] | [ _; "--runner"; "local" ] -> run ~docker:false
  | [ _; "--runner"; "docker" ] -> run ~docker:true
  | _ ->
    prerr_endline "usage: program_tests.exe [--runner local|docker]";
    exit 2
;;
