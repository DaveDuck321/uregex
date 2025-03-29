#define PCRE2_UTF
#define PCRE2_CODE_UNIT_WIDTH 8
#include "pcre2.h"

#include "common.hpp"

#include <cassert>
#include <cstddef>
#include <fcntl.h>
#include <print>
#include <ranges>
#include <string_view>
#include <sys/mman.h>
#include <sys/stat.h>

using namespace std::literals;

static constexpr const char *is_keyword =
    R"(alignas|alignof|and|and_eq|asm|atomic_cancel|atomic_commit|atomic_noexcept|auto|bitand|bitor|bool|break|case|catch|char|char8_t|char16_t|char32_t|class|compl|concept|const|consteval|constexpr|constinit|const_cast|continue|contract_assert|co_await|co_return|co_yield|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|noexcept|not|not_eq|nullptr|operator|or|or_eq|private|protected|public|reflexpr|register|reinterpret_cast|requires|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|synchronized|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while|xor|xor_eq)";

static constexpr const char *is_formatted_as_llvm_function =
    R"([a-z]+([A-Z][a-z]+)+)";

namespace {
auto compile_keyword_regex() -> pcre2_code_8 * {
  uint32_t has_jit = 0;
  pcre2_config(PCRE2_CONFIG_JIT, &has_jit);
  assert(has_jit == 1);

  static int error;
  static PCRE2_SIZE error_offset;
  auto *re =
      pcre2_compile((unsigned char const *)is_keyword, PCRE2_ZERO_TERMINATED,
                    PCRE2_UTF, &error, &error_offset, nullptr);
  assert(re != nullptr);
  std::println("{}", error);
  assert(error == 0);
  return re;
}

auto compile_function_regex() {
  uint32_t has_jit = 0;
  pcre2_config(PCRE2_CONFIG_JIT, &has_jit);
  assert(has_jit == 1);

  static int error;
  static PCRE2_SIZE error_offset;
  auto *re = pcre2_compile((unsigned char const *)is_formatted_as_llvm_function,
                           PCRE2_ZERO_TERMINATED, PCRE2_UTF, &error,
                           &error_offset, nullptr);
  assert(re != nullptr);
  assert(error == 0);
  return re;
}

auto should_highlight(pcre2_code_8 *kw_compiled,
                      pcre2_match_data *kw_match_data,
                      pcre2_code_8 *fn_compiled,
                      pcre2_match_data *fn_match_data, std::string_view line)
    -> bool {
  int rc = pcre2_match(kw_compiled, (const unsigned char *)line.data(),
                       line.size(), 0, 0, kw_match_data, nullptr);
  if (rc != PCRE2_ERROR_NOMATCH) {
    return true;
  }

  rc = pcre2_match(fn_compiled, (const unsigned char *)line.data(), line.size(),
                   0, 0, fn_match_data, nullptr);
  return rc != PCRE2_ERROR_NOMATCH;
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  assert(argc == 2);
  auto test_data = MappedFile(argv[1]);

  auto keyword = compile_keyword_regex();
  auto function = compile_function_regex();

  auto *kw_match_data = pcre2_match_data_create_from_pattern(keyword, NULL);
  auto *fn_match_data = pcre2_match_data_create_from_pattern(function, NULL);

  size_t highlighted = 0;
  for (auto line_view : std::views::split(test_data.underlying, '\n')) {
    std::string_view line = {line_view.data(), line_view.size()};
    if (should_highlight(keyword, kw_match_data, function, fn_match_data,
                         line)) {
      highlighted += 1;
      std::println("Line: {}", line);
    }
  }

  std::println("Highlighted: {}", highlighted);
  return 0;
}
