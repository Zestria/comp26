# Parser performance experiment

Run from the repository root:

```sh
dune exec --profile release MLate/bench/benchmark.exe
```

## Local results

AMD Ryzen 5 7640HS, x86-64, OCaml 5.3.0, native executable:

| Blocks | Bytes | Angstrom (ms) | Menhir (ms) | LL/LR |
| ---: | ---: | ---: | ---: | ---: |
| 1 | 265 | 0.034 | 0.006 | 5.35x |
| 10 | 2659 | 0.379 | 0.065 | 5.86x |
| 100 | 27409 | 5.146 | 1.159 | 4.44x |
| 1000 | 283009 | 63.076 | 12.965 | 4.87x |
