set -eu

# Run from bin/, including inside the Dune cram sandbox.
for tool in ocaml riscv64-linux-gnu-gcc qemu-riscv64 timeout cmp; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    printf 'Required tool not found: %s\n' "$tool" >&2
    exit 1
  fi
done

for example in print_int fac_cps fib_cps; do
  source="../test/riscv/programs/$example.ml"
  case "$example" in
    print_int) answer=42 ;;
    fac_cps) answer=120 ;;
    fib_cps) answer=55 ;;
  esac
  printf '%s' "$answer" > "$example.answer"
  timeout 10s ocaml "$source" > "$example.expected" 2> "$example.ocaml.stderr"
  cmp "$example.answer" "$example.expected"
  test ! -s "$example.ocaml.stderr"

  for parser in topdown menhir; do
    output="${example}_${parser}"
    timeout 10s ../compiler.exe -parsing "$parser" "$source" -o "$output.s" \
      > "$output.compiler.stdout" 2> "$output.compiler.stderr"
    test ! -s "$output.compiler.stdout"
    test ! -s "$output.compiler.stderr"
    timeout 30s riscv64-linux-gnu-gcc \
      -march=rv64gc -mabi=lp64d -std=c11 -Wall -Wextra -Werror \
      -static -no-pie "$output.s" \
      ../runtime/closures/closures.c ../runtime/closures/apply.S \
      ../runtime/io/print_int.c ../runtime/errors.c -o "$output.exe"
    timeout 10s qemu-riscv64 "./$output.exe" \
      > "$output.actual" 2> "$output.stderr"
    if ! cmp "$example.expected" "$output.actual"; then
      printf 'Output mismatch: %s, parser %s\n' "$example" "$parser" >&2
      exit 1
    fi
    test ! -s "$output.stderr"
  done
  printf '%s: both parsers match OCaml (%s).\n' "$example" "$answer"
done
