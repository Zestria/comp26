#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import re
import os
import sys

# I like to use these commands before doing benchmarks:
#
# # Fix the freq of cpu
#  sudo cpupower frequency-set -g performance
#  sudo cpupower frequency-set -d 800Mhz
#  sudo cpupower frequency-set -u 4000Mhz
#
# # Remove turbo
#  echo "1" | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo
#
# # off the Hyper-Threading
# echo off | sudo tee /sys/devices/system/cpu/smt/active
#
# # Isolate 1 2 core as a linux kernel parameter
# isolcpus=2,3
#
# # tick mode whatever as a linux kernel parameter
# nohz_full=2,3
#
# # use isolated cpus
# taskset -c 2, 3 ./my-realtime-program

sample_len = 2


parser_names = ["top_down_parser", "bottom_up_parser"]
colors = ["red", "blue"]
assert len(colors) == len(parser_names)

filename = "log_benchmarking0.log"


def read_samples(path=filename):
    print(f"Using {path}")
    """Read the natural `print_gc` output and return the throughputs.

    Returns a numpy array `samples` of shape
    (num_benchmarks, num_parsers, sample_len), i.e. the `@ .../s` numbers.
    """
    blocks = []
    current = None
    parser = None
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")

            if line.startswith("Throughputs"):
                current = {name: [] for name in parser_names}
                blocks.append(current)
                parser = None
                continue

            if current is None or not line.strip():
                continue

            head = re.match(r"\s*(\w+):", line)
            if head:
                parser = head.group(1)
            if parser is None:
                continue

            value = re.search(r"@\s*([0-9.]+)/s", line)
            if value:
                current[parser].append(float(value.group(1)))

    samples = np.array(
        [[blocks[b][name] for name in parser_names] for b in range(len(blocks))]
    )
    return samples


def parser_dots(samples, parser_index):
    """The plotted dots: mean throughput per benchmark block."""
    return np.mean(samples[:, parser_index, :], axis=1)


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else filename
    samples = read_samples(path)

    if samples.size == 0:
        print("Error: No valid benchmark results.")
        return 1

    rows = []
    for j, name in enumerate(parser_names):
        dots = parser_dots(samples, j)
        mean = float(np.mean(dots))
        median = float(np.median(dots))
        std = float(np.std(dots, ddof=1))
        n = len(dots)
        # 3 sigma confidence interval for the mean, assuming a normal sample.
        ci = 3.0 * std / np.sqrt(n)
        rows.append((name, dots, mean, median, std, ci, n))

    fig = plt.figure(figsize=(12, 7))
    gs = fig.add_gridspec(2, 1, height_ratios=[1, 2], hspace=0.55)

    # Top: the summary table.
    ax_table = fig.add_subplot(gs[0, 0])
    ax_table.axis("off")

    columns = ["parser", "n", "mean", "median", "stddev", "ci (3*SE)"]
    cell_text = []
    for name, dots, mean, median, std, ci, n in rows:
        cell_text.append(
            [name, str(n), f"{mean:.2f}", f"{median:.2f}", f"{std:.2f}", f"± {ci:.2f}"]
        )

    table = ax_table.table(
        cellText=cell_text, colLabels=columns, loc="center", cellLoc="center"
    )
    table.auto_set_font_size(False)
    table.set_fontsize(11)
    table.scale(1, 1.8)
    ax_table.set_title("Benchmark summary", fontsize=13, pad=12)

    # Bottom: sample dots (per-benchmark mean) on one axis per parser,
    # with mean and median markers.
    ax = fig.add_subplot(gs[1, 0])
    for j, (name, dots, mean, median, std, ci, n) in enumerate(rows):
        y = len(parser_names) - 1 - j  # top_down_parser on top
        ax.scatter(
            dots,
            [y] * n,
            color=colors[j],
            s=25,
            alpha=0.55,
            label="sample dots" if j == 0 else None,
        )
        ax.plot(
            [mean, mean],
            [y - 0.3, y + 0.3],
            color=colors[j],
            linewidth=2,
            label="mean" if j == 0 else None,
        )
        ax.plot(
            [median, median],
            [y - 0.3, y + 0.3],
            color=colors[j],
            linewidth=2,
            linestyle=":",
            label="median" if j == 0 else None,
        )
        ax.errorbar(
            mean,
            y,
            xerr=ci,
            fmt="none",
            ecolor=colors[j],
            elinewidth=2,
            capsize=6,
            capthick=2,
            zorder=3,
            label="ci (3*SE)" if j == 0 else None,
        )
        ax.text(
            mean,
            y + 0.38,
            f"mean={mean:.2f}",
            color=colors[j],
            ha="center",
            fontsize=8,
        )
        ax.text(
            median,
            y - 0.45,
            f"median={median:.2f}",
            color=colors[j],
            ha="center",
            fontsize=8,
        )

    ax.set_yticks([0, 1])
    ax.set_yticklabels(["bottom_up_parser", "top_down_parser"])
    ax.set_xlabel("Throughput (items/second)")
    ax.set_title("Sample dots (per-benchmark mean)")
    ax.grid(True, axis="x", alpha=0.3)
    ax.legend(loc="upper right", fontsize=8)

    output_table = "output_table1.png"
    fig.savefig(output_table, bbox_inches="tight")
    print(f"\nTable saved to: {output_table}")
    plt.show()

    return 0


if __name__ == "__main__":
    main()
