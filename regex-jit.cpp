#include "regex/engine.hpp"
#include "regex/parser.hpp"
#include "regex/visualize.hpp"

#include <cassert>
#include <cctype>
#include <iostream>
#include <ostream>
#include <ranges>
#include <string_view>
#include <sys/types.h>

using namespace std::literals;

int main(int argc, char *argv[]) {
  assert(argc == 3);
  std::string_view regex_string = argv[1];
  std::string_view match_string = argv[2];

  // Parse
  auto compiled = regex::parse(regex_string);
  regex::output_graph(std::cout, compiled);

  // Evaluate
  auto result = regex::evaluate(compiled, match_string);
  if (result) {
    std::println(std::cerr, "Match!");
    for (auto [index, group] : std::ranges::views::enumerate(result.groups)) {
      if (group.has_value()) {
        std::println(std::cerr, "  Group[{}]: '{}' ({}-{})", index,
                     match_string.substr(group->start_index, group->end_index),
                     group->start_index, group->end_index);
      } else {
        std::println(std::cerr, "  Group[{}]: None", index);
      }
    }
  } else {
    std::println(std::cerr, "No match :-(");
  }
}
