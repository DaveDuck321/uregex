#include "regex/regex.hpp"

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

static constexpr std::string_view is_keyword =
    R"(alignas|alignof|and|asm|auto|bool|break|case|catch|char|class|const|constexpr|const_cast|continue|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|if|inline|int|long|mutable|namespace|new|noexcept|nullptr|operator|private|protected|public|reinterpret_cast|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while)";

static constexpr std::string_view is_formatted_as_llvm_function =
    R"([a-z]+([A-Z][a-z]+)+)";

namespace {
auto compile_keyword_regex() {
#ifdef USE_JIT
  return uregex::compile(uregex::parse(is_keyword));
#else
  return uregex::parse(is_keyword);
#endif
}

auto compile_function_regex() {
#ifdef USE_JIT
  return uregex::compile(uregex::parse(is_formatted_as_llvm_function));
#else
  return uregex::parse(is_formatted_as_llvm_function);
#endif
}

auto should_highlight(uregex::MatchResult &keyword_match_result,
                      uregex::MatchResult &function_match_result,
                      auto const &keyword_re, auto const &function_re,
                      std::string_view line) -> bool {
  return keyword_re.evaluate(keyword_match_result, line) ||
         function_re.evaluate(function_match_result, line);
}
} // namespace

auto main(int argc, char *argv[]) -> int {
  assert(argc == 2);
  auto test_data = MappedFile(argv[1]);

  uregex::MatchResult keyword_match_result;
  uregex::MatchResult function_match_result;
  auto keyword = compile_keyword_regex();
  auto function = compile_function_regex();

  size_t highlighted = 0;

  for (size_t i = 0; i < 4; i += 1) {
    for (auto line_view : std::views::split(test_data.underlying, '\n')) {
      std::string_view line = {line_view.data(), line_view.size()};
      if (should_highlight(keyword_match_result, function_match_result, keyword,
                           function, line)) {
        highlighted += 1;
      }
    }
  }

  std::println("Highlighted: {}", highlighted);
  return 0;
}
