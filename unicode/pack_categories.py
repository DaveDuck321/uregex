#!/usr/bin/env python3
import struct
import urllib.request
from argparse import ArgumentParser
from pathlib import Path
from typing import Iterator

all_categories = {
    "Ll",
    "Lm",
    "Lo",
    "Lt",
    "Lu",
    "Mc",
    "Me",
    "Mn",
    "Nd",
    "Sm",
    "Nl",
    "No",
    "Pc",
    "Pd",
    "Pe",
    "Pf",
    "Pi",
    "Po",
    "Ps",
    "Zl",
    "Zp",
    "Zs",
    "Sc",
    "Sk",
    "So",
    "Cc",
    "Cf",
    "Cn",
    "Co",
    "Cs",
}


def download_unicode_data(version: str) -> Iterator[str]:
    ucd_url = f"https://www.unicode.org/Public/{version}/ucd/UnicodeData.txt"
    with urllib.request.urlopen(ucd_url) as file:
        for line in file:
            assert isinstance(line, bytes)
            yield line.decode("utf-8")


def get_info(line: str) -> tuple[int, str, str]:
    parts = line.split(";")
    codepoint_hex = parts[0]
    name = parts[1]
    category = parts[2]
    assert category in all_categories, category

    return int(codepoint_hex, 16), name, category


def get_category_map(lines: Iterator[str]) -> dict[int, str]:
    result: dict[int, str] = {}

    repeat_category: str | None = None
    last_codepoint: int = 0
    for line in lines:
        codepoint, name, category = get_info(line)

        if name.endswith(", Last>"):
            assert repeat_category is not None
            assert repeat_category == category

            for new_codepoint in range(last_codepoint + 1, codepoint):
                result[new_codepoint] = repeat_category

        if name.endswith(", First>"):
            repeat_category = category
        else:
            repeat_category = None

        result[codepoint] = category
        last_codepoint = codepoint

    return result


def output_category_map(character_map: dict[int, str], output: Path) -> None:
    with output.open("wb") as output_file:
        assert max(character_map) < 0x110000
        for codepoint in range(0x110000):
            category = character_map.get(codepoint, "Cu")
            output_file.write(category.encode("utf-8"))


if __name__ == "__main__":
    parser = ArgumentParser("pack_categories")
    parser.add_argument("--version", type=str, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    unicode_data = download_unicode_data(args.version)
    category_map = get_category_map(unicode_data)
    output_category_map(category_map, args.output)
