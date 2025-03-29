#include <ctre-unicode.hpp>

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

// We have to fragment into multiple regex since the string to too long to
// compile: instantiating fold expression with 750 arguments exceeded expression
// nesting limit of 256
static constexpr auto is_keyword_1 = ctre::match<
    R"(alignas|alignof|and|and_eq|asm|atomic_cancel|atomic_commit|atomic_noexcept|auto|bitand|bitor|bool|break|case|catch|char|char8_t|char16_t|char32_t|class|compl|concept|const|consteval|constexpr|constinit|const_cast|continue|contract_assert|co_await|co_return)">;

static constexpr auto is_keyword_2 = ctre::match<
    R"(co_yield|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|noexcept|not|not_eq|nullptr|operator|or|or_eq|private|protected|public)">;

static constexpr auto is_keyword_3 = ctre::match<
    R"(reflexpr|register|reinterpret_cast|requires|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|synchronized|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile)">;

static constexpr auto is_keyword_4 = ctre::match<R"(wchar_t|while|xor|xor_eq)">;

static constexpr auto is_formatted_as_llvm_function =
    ctre::match<R"([a-z]+([A-Z][a-z]+)+)">;

namespace {
auto should_highlight(std::u8string_view line) -> bool {
  return is_keyword_1(line) || is_keyword_2(line) || is_keyword_3(line) ||
         is_keyword_4(line) || is_formatted_as_llvm_function(line);
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  assert(argc == 2);
  auto test_data = MappedFile(argv[1]);

  size_t highlighted = 0;
  for (auto line_view : std::views::split(test_data.underlying, '\n')) {
    std::string_view line = {line_view.data(), line_view.size()};
    if (should_highlight(
            std::u8string_view((const char8_t *)line.data(), line.size()))) {
      highlighted += 1;
    }
  }

  std::println("Highlighted: {}", highlighted);
  return 0;
}
