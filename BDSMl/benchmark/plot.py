#!/usr/bin/env python3

import argparse
import csv
import os
import statistics
from collections import defaultdict
from pathlib import Path

BENCHMARK_DIR = Path(__file__).resolve().parent
DEFAULT_INPUT = BENCHMARK_DIR / "benchmark_results.csv"
DEFAULT_OUTPUT = BENCHMARK_DIR / "parser_benchmark.png"
os.environ.setdefault("MPLCONFIGDIR", str(BENCHMARK_DIR / ".matplotlib"))

import matplotlib.pyplot as plt


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Plot LL and LR parser wall time.")
    parser.add_argument("input", nargs="?", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("-o", "--output", type=Path, default=DEFAULT_OUTPUT)
    return parser.parse_args()


def read_measurements(path: Path):
    measurements = defaultdict(list)
    with path.open(newline="", encoding="utf-8") as source:
        for row in csv.DictReader(source):
            key = row["dataset"], row["parser"], int(row["size"])
            measurements[key].append(
                float(row["wall_seconds_per_parse"]) * 1_000_000
            )
    return measurements


def main() -> None:
    args = parse_args()
    measurements = read_measurements(args.input)
    datasets = sorted({dataset for dataset, _, _ in measurements})
    if not datasets:
        raise SystemExit(f"no measurements found in {args.input}")

    figure, axes = plt.subplots(
        1, len(datasets), figsize=(5 * len(datasets), 4), squeeze=False
    )
    for axis, dataset in zip(axes[0], datasets):
        for parser, marker in (("LL", "o"), ("LR", "s")):
            sizes = sorted(
                size
                for current_dataset, current_parser, size in measurements
                if current_dataset == dataset and current_parser == parser
            )
            means = [statistics.mean(measurements[dataset, parser, size]) for size in sizes]
            deviations = [
                statistics.stdev(measurements[dataset, parser, size])
                if len(measurements[dataset, parser, size]) > 1
                else 0.0
                for size in sizes
            ]
            axis.errorbar(
                sizes,
                means,
                yerr=deviations,
                label=parser,
                marker=marker,
                capsize=3,
            )

        axis.set_title(dataset)
        axis.set_xlabel("Input size")
        axis.set_ylabel("Wall time per parse, us")
        axis.set_xscale("log")
        axis.set_yscale("log")
        axis.grid(True, which="both", alpha=0.3)
        axis.legend()

    figure.suptitle("Recursive descent (LL) vs Menhir (LR)")
    figure.tight_layout()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(args.output, dpi=200)
    print(f"Plot written to {args.output}")


if __name__ == "__main__":
    main()
