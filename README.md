# µregex

A lightweight, extremely fast UTF-8 regex matching library with optional evaluation-via-JIT.

## Goals
1. Evaluation must be as-fast-as-possible -- in all cases
    1. Even without the JIT
    2. Even with an adversarial regex -- we must be polynomial
3. Compile time overhead must be tiny (iteration time is important).
4. No dependencies outside of the standard library (a regex library is a primitive).

## Building

Note: I do not distribute the [unicode category database](https://www.unicode.org/Public/16.0.0/ucd/UnicodeData.txt) so it is downloaded in during the build process.
```fish
> make -j build/libregex.O3.a
```

## Usage

There is no configuration, unicode is always enabled, match groups are always produced. Users can optionally call `compile()` to enable JIT evaluation for a ~2.5x performance gain during evaluation.

Pull in the header:
```c++
#include "regex/regex.hpp"

#include <cassert>
#include <cctype>
#include <iostream>
#include <ostream>
#include <ranges>
#include <string_view>
#include <sys/types.h>

using namespace std::literals;

namespace {
auto summarize_result(regex::MatchResult result) -> void {
  if (result) {
    std::println(std::cerr, "Match!");
    for (auto [index, group] : std::ranges::views::enumerate(result.groups)) {
      if (group.has_value()) {
        std::println(std::cerr, "  Group[{}]: '{}' ({}-{})", index,
                     result.text.substr(group->start_index, group->end_index),
                     group->start_index, group->end_index);
      } else {
        std::println(std::cerr, "  Group[{}]: None", index);
      }
    }
  } else {
    std::println(std::cerr, "No match :-(");
  }
}
} // namespace

int main(int argc, char *argv[]) {
  assert(argc == 3);
  std::string_view regex_string = argv[1];
  std::string_view match_string = argv[2];

  // Parse
  auto graph = regex::parse(regex_string);
  graph.visualize(std::cout);

  // Cpp implementation
  summarize_result(graph.evaluate(match_string));

  // JIT
  auto compiled_graph = regex::compile(std::move(graph));
  summarize_result(compiled_graph.evaluate(match_string));
}

```

Link with the static library:
```fish
> g++ -std=c++26 -I/path/to/uregex/include build/libregex.O3.a my_program.cpp
```
