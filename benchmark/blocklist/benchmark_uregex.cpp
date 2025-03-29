#include "regex/regex.hpp"

#include "common.hpp"

#include <cassert>
#include <cstddef>
#include <fcntl.h>
#include <iostream>
#include <ostream>
#include <print>
#include <ranges>
#include <string_view>
#include <sys/mman.h>
#include <sys/stat.h>

using namespace std::literals;

namespace {
auto compile_regex() {
#ifdef USE_JIT
  return uregex::compile(uregex::parse(R"(.+\.(\w+))"));
#else
  return uregex::parse(R"(.+\.(\w+))");
#endif
}

auto is_dot_com(uregex::MatchResult &match_result, auto const &graph,
                std::string_view line) -> bool {
  return graph.evaluate(match_result, line) &&
         match_result.group_text(0) == "com"sv;
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  assert(argc == 2);
  auto test_data = MappedFile(argv[1]);

  uregex::MatchResult match_result;
  auto compiled = compile_regex();

  size_t dot_coms = 0;
  for (size_t i = 0; i < 2; i += 1) {
    for (auto line_view : std::views::split(test_data.underlying, '\n')) {
      std::string_view line = {line_view.data(), line_view.size()};
      if (is_dot_com(match_result, compiled, line)) {
        dot_coms += 1;
      }
    }
  }

  std::println(std::cout, ".coms: {}", dot_coms);
  return 0;
}
