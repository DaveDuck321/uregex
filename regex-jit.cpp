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
