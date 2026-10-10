set -eu

for source in fac_cps.ml fib_cps.ml closure_input.ml nested_functions.ml shadowing.ml short_circuit.ml if_argument.ml recursive_capture.ml evaluation_order.ml; do
  expected=$(ocaml "$source")
  for parser in topdown menhir; do
    for stage in parsing alpha cc ll anf; do
      output="${source%.ml}_${parser}_${stage}.ml"
      ../compiler.exe -parsing "$parser" --stop-after "$stage" "$source" -o "$output"
      actual=$(ocaml "$output")
      if [ "$actual" != "$expected" ]; then
        printf 'Mismatch: %s, %s, %s: expected %s, got %s\n' "$source" "$parser" "$stage" "$expected" "$actual" >&2
        exit 1
      fi
    done
  done
done

echo 'All stages preserve the output of all examples.'
