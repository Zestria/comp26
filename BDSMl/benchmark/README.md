# Бенчмарк парсеров

В этой директории находится весь код эксперимента LL против Menhir/LR:

- `parser_bench.ml` генерирует входы, проверяет равенство AST и измеряет wall-time;
- `benchmark_results.csv` содержит сырые измерения;
- `summarize.py` строит сводную CSV-таблицу;
- `plot.py` строит PNG-график;
- `requirements.txt` содержит зависимость для визуализации.

Из директории `BDSMl` запустите:

```sh
dune exec --profile release benchmark/parser_bench.exe
python3 benchmark/summarize.py
python3 -m pip install -r benchmark/requirements.txt
python3 benchmark/plot.py
```

Результаты создаются рядом со скриптами:

```text
benchmark/benchmark_results.csv
benchmark/benchmark_summary.csv
benchmark/parser_benchmark.png
```

Бенчмарк всегда использует стандартную конфигурацию LL, совпадающую с Menhir.
Экспериментальные переопределения ассоциативности не участвуют в сравнении
производительности.
