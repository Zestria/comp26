#!/usr/bin/env python3

import argparse
import csv
import statistics
from collections import defaultdict
from pathlib import Path


BENCHMARK_DIR = Path(__file__).resolve().parent
DEFAULT_INPUT = BENCHMARK_DIR / "benchmark_results.csv"
DEFAULT_OUTPUT = BENCHMARK_DIR / "benchmark_summary.csv"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Aggregate raw LL/LR parser benchmark measurements."
    )
    parser.add_argument("input", nargs="?", type=Path, default=DEFAULT_INPUT)
    parser.add_argument("-o", "--output", type=Path, default=DEFAULT_OUTPUT)
    return parser.parse_args()


def read_groups(path: Path):
    groups = defaultdict(list)
    metadata = {}
    with path.open(newline="", encoding="utf-8") as source:
        for row in csv.DictReader(source):
            key = row["dataset"], int(row["size"]), row["parser"]
            groups[key].append(float(row["wall_seconds_per_parse"]) * 1_000_000)
            metadata[key] = int(row["source_bytes"])
    return groups, metadata


def summarize(groups, metadata):
    for key in sorted(groups):
        values = groups[key]
        dataset, size, parser = key
        yield {
            "dataset": dataset,
            "size": size,
            "source_bytes": metadata[key],
            "parser": parser,
            "samples": len(values),
            "mean_us": statistics.mean(values),
            "median_us": statistics.median(values),
            "stdev_us": statistics.stdev(values) if len(values) > 1 else 0.0,
            "min_us": min(values),
            "max_us": max(values),
        }


def main() -> None:
    args = parse_args()
    groups, metadata = read_groups(args.input)
    rows = list(summarize(groups, metadata))
    if not rows:
        raise SystemExit(f"no measurements found in {args.input}")

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("w", newline="", encoding="utf-8") as target:
        writer = csv.DictWriter(target, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)

    header = f"{'dataset':<13} {'size':>6} {'parser':>6} {'mean, us':>12} {'std, us':>12}"
    print(header)
    print("-" * len(header))
    for row in rows:
        print(
            f"{row['dataset']:<13} {row['size']:>6} {row['parser']:>6} "
            f"{row['mean_us']:>12.3f} {row['stdev_us']:>12.3f}"
        )
    print(f"\nSummary written to {args.output}")


if __name__ == "__main__":
    main()
