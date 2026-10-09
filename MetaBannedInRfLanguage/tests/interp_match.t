AST and ANF interpreters agree on pattern matching

  $ cat > match_example.ml <<EOF
  > let classify n = match n with
  >   | 0 -> false
  >   | 1 -> true
  >   | _ -> n > 1
  > ;;
  > let r1 = classify 0;;
  > let r2 = classify 5
  > EOF

  $ ./interp_check.exe match_example.ml
  classify = <closure>
  r1 = false
  r2 = true

  $ ./interp_check.exe --full match_example.ml
  classify = <closure>
  r1 = false
  r2 = true
