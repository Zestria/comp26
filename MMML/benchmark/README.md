# Parser benchmark

Run from the repository root:

```sh
dune build @benchmark --force
```

Results (milliseconds per parse; negative difference means Menhir was faster):

```text
+-------------+-------------+----------------+----------------+------------+
| Definitions | Source (B)  |  Top-down (ms) | Buttom-Up (ms) | Difference |
+-------------+-------------+----------------+----------------+------------+
|       1,000 |     360,070 |          9.494 |          6.585 |    -30.65% |
|       5,000 |   1,789,841 |         46.356 |         34.440 |    -25.71% |
|      10,000 |   3,576,738 |         91.337 |         67.423 |    -26.18% |
+-------------+-------------+----------------+----------------+------------+
```

CPU: 11th Gen Intel Core i5-11400H @ 2.70GHz   
OS: Ubuntu 26.04.1 LTS (Linux 7.0.0-34-generic)   
Compiler: OCaml 5.5.1 