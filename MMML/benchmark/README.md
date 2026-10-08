# Parser benchmark

Run from the repository root:

```sh
dune build @benchmark --force
```

Results (milliseconds per parse; negative difference means Menhir was faster):

```text
+-------------+-------------+----------------+----------------+------------+
| Definitions | Source (B)  |  Top-down (ms) | Bottom-up (ms) | Difference |
+-------------+-------------+----------------+----------------+------------+
|       1,000 |     364,571 |          8.616 |          6.119 |    -28.98% |
|       5,000 |   1,816,273 |         41.763 |         30.918 |    -25.97% |
|      10,000 |   3,629,607 |         84.277 |         63.531 |    -24.62% |
+-------------+-------------+----------------+----------------+------------+
```

CPU: 11th Gen Intel Core i5-11400H @ 2.70GHz   
OS: Ubuntu 26.04.1 
Compiler: OCaml 5.5.1 