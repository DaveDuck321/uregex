#include "common.hpp"

#include <cassert>
#include <cstddef>
#include <fcntl.h>
#include <iostream>
#include <ostream>
#include <print>
#include <ranges>
#include <regex>
#include <string_view>
#include <sys/mman.h>
#include <sys/stat.h>

using namespace std::literals;

namespace {
auto compile_regex() -> std::regex { return std::regex(R"(.+\.(\w+))"); }

auto is_dot_com(std::regex &graph, std::smatch &match, std::string line)
    -> bool {
  if (not std::regex_match(line, match, graph)) {
    return false;
  }
  return match[0] == "com";
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  assert(argc == 2);
  auto test_data = MappedFile(argv[1]);

  std::smatch match_state;
  auto compiled = compile_regex();

  size_t dot_coms = 0;
  for (auto line_view : std::views::split(test_data.underlying, '\n')) {
    std::string_view line = {line_view.data(), line_view.size()};
    if (is_dot_com(compiled, match_state, std::string{line})) {
      dot_coms += 1;
    }
  }

  std::println(std::cout, ".coms: {}", dot_coms);
  return 0;
}
