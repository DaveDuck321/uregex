#!/usr/bin/env python3
import urllib.request
import random
from argparse import ArgumentParser
from itertools import groupby
from pathlib import Path


def fetch_blocklist(output_dir: Path):
    blocklist_output_dir = output_dir / "blocklist"
    blocklist_output_dir.mkdir(exist_ok=True, parents=True)

    url = "https://big.oisd.nl/domainswild2"
    blocklist: list[str] = []
    with urllib.request.urlopen(url) as file:
        for line in file:
            line = line.decode("utf-8").strip()
            if line.startswith("#") or not line:
                continue

            blocklist.append(line)

    size = 128
    while size < len(blocklist):
        size *= 2
        with (blocklist_output_dir / f"blocklist-{size:x}.txt").open("w") as file:
            file.write("\n".join(sorted(random.choices(blocklist, k=size))))


def fetch_llvm(output_dir: Path):
    llvm_output_dir = output_dir / "llvm"
    llvm_output_dir.mkdir(exist_ok=True, parents=True)

    url = "https://raw.githubusercontent.com/llvm/llvm-project/refs/heads/main/llvm/lib/CodeGen/SelectionDAG/LegalizeDAG.cpp"
    tokens: list[str] = []
    with urllib.request.urlopen(url) as file:
        for line in file:
            line = line.decode("utf-8").strip()
            if line.startswith("#") or not line:
                continue

            tokens.extend("".join(group[1]) for group in groupby(line, key=str.isalnum))

    tokens = list(filter(lambda token: len(token) > 2, tokens))

    size = 128
    while size < len(tokens):
        size *= 2
        with (llvm_output_dir / f"llvm-{size:x}.txt").open("w") as file:
            file.write("\n".join(random.choices(tokens, k=size)))


if __name__ == "__main__":
    random.seed(0)

    parser = ArgumentParser("fetch_resources")
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()

    fetch_blocklist(args.output_dir)
    # fetch_llvm(args.output_dir)
