#include "regex/engine.hpp"
#include "regex/parser.hpp"
#include "testing.hpp"

#include <cctype>
#include <cstdlib>
#include <sys/types.h>

using namespace std::literals;

namespace {
auto does_match(std::string_view regex, std::string_view string) -> bool {
  auto compiled = regex::parse(regex);
  return regex::evaluate(compiled, string);
}
} // namespace

TEST_CASE(basic_digit, "[regex][groups]") {
  CHECK(does_match(R"(\d)", "7"));
  CHECK(!does_match(R"(\d)", "a"));
  CHECK(!does_match(R"(\d)", "_"));

  CHECK(!does_match(R"(\D)", "7"));
  CHECK(does_match(R"(\D)", "a"));
  CHECK(does_match(R"(\D)", "_"));
}

TEST_CASE(basic_whitespace, "[regex][groups]") {
  CHECK(does_match(R"(\s)", "\t"));
  CHECK(does_match(R"(\s)", " "));
  CHECK(does_match(R"(\s)", "\n"));
  CHECK(does_match(R"(\s)", "\r"));
  CHECK(does_match(R"(\s)", " "));

  CHECK(!does_match(R"(\s)", "_"));
  CHECK(does_match(R"(\S)", "9"));
  CHECK(!does_match(R"(\S)", "\t"));
}

TEST_CASE(basic_alphanumeric, "[regex][groups]") {
  CHECK(does_match(R"(\w+)", "_09abcABC"));
  CHECK(!does_match(R"(\w+)", "Ā"));
  CHECK(does_match(R"(\W)", " "));
}
