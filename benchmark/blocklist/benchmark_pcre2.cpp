#define PCRE2_UTF
#define PCRE2_CODE_UNIT_WIDTH 8
#include "pcre2.h"

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
auto compile_regex() -> pcre2_code_8 * {
  uint32_t has_jit = 0;
  pcre2_config(PCRE2_CONFIG_JIT, &has_jit);
  assert(has_jit == 1);

  static int error;
  static PCRE2_SIZE error_offset;
  auto *re = pcre2_compile((const unsigned char *)R"(.+\.(\w+))",
                           PCRE2_ZERO_TERMINATED, PCRE2_UTF, &error,
                           &error_offset, nullptr);
  assert(re != nullptr);
  assert(error == 0);
  return re;
}

auto is_dot_com(pcre2_code_8 *compiled, pcre2_match_data *match_data,
                std::string_view line) -> bool {
  int rc = pcre2_match(compiled, (const unsigned char *)line.data(),
                       line.size(), 0, 0, match_data, nullptr);
  if (rc == PCRE2_ERROR_NOMATCH) {
    return false;
  }

  assert(!(rc < 0));
  PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
  auto result = line.substr(ovector[2], ovector[1] - ovector[2]) == "com";
  return result;
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  assert(argc == 2);
  auto test_data = MappedFile(argv[1]);

  auto *re = compile_regex();
  auto *match_data = pcre2_match_data_create_from_pattern(re, NULL);

  size_t dot_coms = 0;
  for (auto line_view : std::views::split(test_data.underlying, '\n')) {
    std::string_view line = {line_view.data(), line_view.size()};
    if (is_dot_com(re, match_data, line)) {
      dot_coms += 1;
    }
  }

  std::println(std::cout, ".coms: {}", dot_coms);
  return 0;
}
