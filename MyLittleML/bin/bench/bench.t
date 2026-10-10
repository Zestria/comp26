Check that the benchmark prints a well-formed table: a header line plus
one row per input family, with numeric size/bytes/times/ratio columns.
Exact times and ratios vary between machines, so only the format is
checked (grep counts the well-formed rows).

  $ ./main.exe > bench.out && wc -l < bench.out && head -1 bench.out && grep -cE '^(deep-let|flat-sum|many-decls)[ ]+[0-9]+[ ]+[0-9]+[ ]+[0-9.]+[ ]+[0-9.]+[ ]+[0-9.]+$' bench.out
  10
  input          size    bytes   topdown(s)  bottomup(s)  ratio(td/bu)
  9
