open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open BDSMl_backend
module Scan = Allocation.Linear_scan
module Frame = Allocation.Frame
open Assembly.Asm

let interval name start finish = Scan.{ name; start; finish }
let assignment name assignments = List.assoc name assignments

let reuse_registers () =
  let assigned = Scan.allocate ~registers:[ S0 ] [ interval "x" 0 1; interval "y" 2 3 ] in
  Alcotest.(check bool) "expired register reused" true
    (assignment "x" assigned = Scan.Register S0 && assignment "y" assigned = Scan.Register S0)
;;

let spill_furthest () =
  let assigned = Scan.allocate ~registers:[ S0 ] [ interval "long" 0 100; interval "short" 1 2 ] in
  Alcotest.(check bool) "long interval evicted" true (assignment "long" assigned = Scan.Spill 0);
  Alcotest.(check bool) "short interval kept" true (assignment "short" assigned = Scan.Register S0);
  let assigned = Scan.allocate ~registers:[ S0 ] [ interval "short" 0 2; interval "long" 1 100 ] in
  Alcotest.(check bool) "new long interval spills" true (assignment "long" assigned = Scan.Spill 0)
;;

let overlapping_endpoints () =
  let assigned = Scan.allocate ~registers:[ S0 ] [ interval "x" 0 1; interval "y" 1 2 ] in
  Alcotest.(check bool) "same instruction cannot share a register" true
    (assignment "x" assigned <> assignment "y" assigned)
;;

let branches_and_unused () =
  let body =
    ALet (PVar "result",
      CIf (ImmBool true,
        ALet (PVar "local", CImm (ImmInt 1), ACExpr (CBinOp (Plus, ImmId "local", ImmId "outer"))),
        ACExpr (CImm (ImmId "outer"))),
      ACExpr (CBinOp (Plus, ImmId "result", ImmId "outer")))
  in
  let intervals = Scan.intervals [ PVar "outer"; PVar "unused" ] [ body ] in
  let find name = List.find (fun interval -> interval.Scan.name = name) intervals in
  Alcotest.(check bool) "outer spans both branches and join" true
    ((find "outer").finish > (find "result").start);
  Alcotest.(check bool) "definition follows branch computations" true
    ((find "result").start > (find "local").finish);
  Alcotest.(check bool) "unused parameter excluded" false
    (List.exists (fun interval -> interval.Scan.name = "unused") intervals)
;;

let pressure_and_determinism () =
  let random = Random.State.make [| 2631 |] in
  for sample = 0 to 199 do
    let intervals = List.init 60 (fun i ->
      let start = Random.State.int random 100 in
      interval (string_of_int i) start (start + Random.State.int random 40)) in
    let assigned = Scan.allocate ~registers:[ S0; S1; S2 ] intervals in
    Alcotest.(check bool) "independent deterministic allocation" true
      (assigned = Scan.allocate ~registers:[ S0; S1; S2 ] intervals);
    List.iter (fun left -> List.iter (fun right ->
      if left.Scan.name <> right.Scan.name
         && left.start <= right.finish && right.start <= left.finish then
        Alcotest.(check bool) (Printf.sprintf "no interference (sample %d)" sample) false
          (assignment left.name assigned = assignment right.name assigned)) intervals) intervals
  done
;;

let no_register_pool () =
  let assigned = Scan.allocate ~registers:[] [ interval "x" 0 1 ] in
  Alcotest.(check bool) "zero registers spills" true (assignment "x" assigned = Scan.Spill 0)
;;

let frame_unused () =
  let frame = Frame.create [ PVar "unused" ] [ ACExpr (CImm (ImmInt 42)) ] in
  Alcotest.(check bool) "unused binding" true (Frame.location frame "unused" = Frame.Unused);
  Alcotest.(check int) "no save of unused s-registers" 0 (List.length (Frame.saved_registers frame));
  Alcotest.(check int) "no stack slots for unused locals" 0 (Frame.spill_count frame)
;;

let () =
  Alcotest.run "linear scan"
    [ "allocation",
      [ Alcotest.test_case "expire and reuse" `Quick reuse_registers
      ; Alcotest.test_case "spill furthest end" `Quick spill_furthest
      ; Alcotest.test_case "overlapping endpoints" `Quick overlapping_endpoints
      ; Alcotest.test_case "branches and unused" `Quick branches_and_unused
      ; Alcotest.test_case "random interference and determinism" `Quick pressure_and_determinism
      ; Alcotest.test_case "empty register pool" `Quick no_register_pool
      ; Alcotest.test_case "unused frame" `Quick frame_unused ] ]
;;
