[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let repeat count text = String.concat "" (List.init count (fun _ -> text))

let cps count =
  repeat
    count
    {|let rec factorial n k =
  if n = 0 then k 1 else factorial (n - 1) (fun r -> k (n * r))
let rec fibonacci n k =
  if n < 2 then k n
  else fibonacci (n - 1) (fun a -> fibonacci (n - 2) (fun b -> k (a + b)))
;; factorial 10 (fun x -> x);;
fibonacci 10 (fun x -> x);;
|}
;;

let application count = "f " ^ repeat count "x "
let operators count = "x" ^ repeat count " + y * z"
let nested_comments count = repeat count "(*" ^ "body" ^ repeat count "*)" ^ " 1"

let quoted_comment count =
  let delimiter = String.make count 'a' in
  "(* {" ^ delimiter ^ "|" ^ String.make count '|' ^ "|" ^ delimiter ^ "} *) 1"
;;

type expectation =
  | Accepted
  | Rejected

let cases =
  [ "cps", cps, Accepted
  ; "application", application, Accepted
  ; "operators", operators, Accepted
  ; "nested_comments", nested_comments, Accepted
  ; "quoted_comment", quoted_comment, Accepted
  ; "unfinished_cps", (fun count -> cps count ^ "let unfinished ="), Rejected
  ]
;;

let validate expected source =
  match
    ( expected
    , Bkml_frontend.Parser.parse_program source
    , Bkml_frontend.Menhir_parser.parse_program source )
  with
  | Accepted, Ok left, Ok right when left = right -> Ok ()
  | Accepted, Ok _, Ok _ -> Error "Parsers produced different ASTs"
  | Accepted, Error message, _ -> Error ("Angstrom rejected input: " ^ message)
  | Accepted, _, Error message -> Error ("Menhir rejected input: " ^ message)
  | Rejected, Error _, Error _ -> Ok ()
  | Rejected, Ok _, _ -> Error "Angstrom accepted unfinished input"
  | Rejected, _, Ok _ -> Error "Menhir accepted unfinished input"
;;

let measure iterations parse source =
  let rec loop remaining =
    if remaining > 0
    then (
      let (_ : (Bkml_frontend.Ast.program, string) result) =
        Sys.opaque_identity (parse source)
      in
      loop (remaining - 1))
  in
  Gc.full_major ();
  let start = Sys.time () in
  loop iterations;
  Sys.time () -. start
;;

let target_batch_seconds = 0.1
let sample_count = 6

let calibrate parse source =
  let rec grow iterations =
    let elapsed = measure iterations parse source in
    if elapsed >= target_batch_seconds
    then Ok iterations
    else if iterations > max_int / 2
    then Error "Cannot increase the iteration count further"
    else grow (iterations * 2)
  in
  grow 1
;;

type summary =
  { median : float
  ; minimum : float
  ; maximum : float
  }

let summarize samples =
  let sorted = List.sort Float.compare samples in
  let count = List.length sorted in
  match
    sorted, List.nth_opt sorted ((count - 1) / 2), List.nth_opt sorted (count / 2)
  with
  | minimum :: rest, Some lower, Some upper ->
    Ok
      { median = (lower +. upper) /. 2.
      ; minimum
      ; maximum = List.fold_left Float.max minimum rest
      }
  | _ -> Error "No benchmark samples"
;;

let run_case (name, generate, expected) size =
  let ( let* ) = Result.bind in
  let source = generate size in
  let run () =
    let* () = validate expected source in
    let* angstrom_iterations = calibrate Bkml_frontend.Parser.parse_program source in
    let* menhir_iterations = calibrate Bkml_frontend.Menhir_parser.parse_program source in
    let sample index label iterations parse =
      let batch_ms = 1000. *. measure iterations parse source in
      let parse_ms = batch_ms /. float_of_int iterations in
      Printf.printf
        "%s,%s,%d,%d,%d,%s,%d,%.6f,%.6f\n%!"
        Sys.ocaml_version
        name
        size
        (String.length source)
        index
        label
        iterations
        batch_ms
        parse_ms;
      parse_ms
    in
    let rec collect index angstrom_samples menhir_samples =
      if index > sample_count
      then (
        let* angstrom = summarize angstrom_samples in
        let* menhir = summarize menhir_samples in
        Printf.eprintf
          "%s/%d: Angstrom %.6f [%.6f, %.6f]; Menhir %.6f [%.6f, %.6f]\n%!"
          name
          size
          angstrom.median
          angstrom.minimum
          angstrom.maximum
          menhir.median
          menhir.minimum
          menhir.maximum;
        Ok ())
      else (
        let measure_angstrom () =
          sample index "angstrom" angstrom_iterations Bkml_frontend.Parser.parse_program
        in
        let measure_menhir () =
          sample
            index
            "menhir"
            menhir_iterations
            Bkml_frontend.Menhir_parser.parse_program
        in
        let angstrom, menhir =
          if index mod 2 = 1
          then (
            let angstrom = measure_angstrom () in
            let menhir = measure_menhir () in
            angstrom, menhir)
          else (
            let menhir = measure_menhir () in
            let angstrom = measure_angstrom () in
            angstrom, menhir)
        in
        collect (index + 1) (angstrom :: angstrom_samples) (menhir :: menhir_samples))
    in
    collect 1 [] []
  in
  Result.map_error
    (fun message -> Printf.sprintf "%s, size %d: %s" name size message)
    (run ())
;;

let run () =
  let ( let* ) = Result.bind in
  Printf.eprintf
    "OCaml %s; %d samples; calibration target %.0f ms CPU per batch\n\
     Summary: median [minimum, maximum], CPU milliseconds per parse\n\
     %!"
    Sys.ocaml_version
    sample_count
    (1000. *. target_batch_seconds);
  print_endline
    "ocaml_version,case,size,bytes,sample,parser,iterations,batch_cpu_ms,parse_cpu_ms";
  List.fold_left
    (fun result case ->
       let* () = result in
       List.fold_left
         (fun result size ->
            let* () = result in
            run_case case size)
         (Ok ())
         [ 10; 100; 1_000 ])
    (Ok ())
    cases
;;

let () =
  match run () with
  | Ok () -> ()
  | Error message ->
    prerr_endline message;
    exit 1
;;
