#include <ctre-unicode.hpp>

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
auto is_dot_com(std::u8string_view line) -> bool {
  auto result = ctre::match<R"(^.+\.(\w+)$)">(line);
  return result.matched() && result.get<1>().to_view() ==
                                 std::u8string_view((const char8_t *)"com");
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  auto test_data = MappedFile(argv[1]);

  size_t dot_coms = 0;
  for (auto line_view : std::views::split(test_data.underlying, '\n')) {
    std::string_view line = {line_view.data(), line_view.size()};
    if (is_dot_com(
            std::u8string_view((const char8_t *)line.data(), line.size()))) {
      dot_coms += 1;
    }
  }

  std::println(std::cout, ".coms: {}", dot_coms);
  return 0;
}
