#include "regex/engine.hpp"
#include "regex/parser.hpp"
#include "testing.hpp"

#include <cctype>
#include <cstdlib>
#include <map>
#include <string_view>
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

TEST_CASE(unicode_categories, "[regex][groups][unicode]") {
  const std::map<std::string, regex::Codepoint> examples_from_category = {
      {"Cc", regex::Codepoint{0x0085}}, {"Cf", regex::Codepoint{0x206D}},
      {"Ll", regex::Codepoint{"ꮉ"}},    {"Lm", regex::Codepoint{"𖭂"}},
      {"Lo", regex::Codepoint{"ࠈ"}},    {"Lt", regex::Codepoint{"ᾮ"}},
      {"Lu", regex::Codepoint{"A"}},    {"Mc", regex::Codepoint{"ꦾ"}},
      {"Me", regex::Codepoint{"꙲"}},     {"Mn", regex::Codepoint{0x094D}},
      {"Nd", regex::Codepoint{"🯷"}},    {"Nl", regex::Codepoint{"Ⅻ"}},
      {"No", regex::Codepoint{"㈥"}},   {"Pc", regex::Codepoint{"︴"}},
      {"Pd", regex::Codepoint{"-"}},    {"Pe", regex::Codepoint{")"}},
      {"Pf", regex::Codepoint{"⸡"}},    {"Pi", regex::Codepoint{"“"}},
      {"Po", regex::Codepoint{"@"}},    {"Ps", regex::Codepoint{"("}},
      {"Sc", regex::Codepoint{"$"}},    {"Sk", regex::Codepoint{"﮶"}},
      {"Sm", regex::Codepoint{"⅀"}},    {"So", regex::Codepoint{"⌁"}},
      {"Zl", regex::Codepoint{0x2028}}, {"Zp", regex::Codepoint{0x2029}},
      {"Zs", regex::Codepoint{" "}},
  };

  // Assert that each example matches one and ONLY one category
  for (auto const &[test_category, matching_codepoint] :
       examples_from_category) {
    auto regex = std::format("\\p{{{}}}", test_category);
    auto complement_regex = std::format("\\P{{{}}}", test_category);

    auto compiled = regex::parse(regex);
    auto complied_complement = regex::parse(complement_regex);

    for (auto const &[_, other_codepoint] : examples_from_category) {
      std::string test_string;
      regex::codepoint_to_utf8(test_string, other_codepoint);

      if (matching_codepoint == other_codepoint) {
        CHECK(regex::evaluate(compiled, test_string));
        CHECK(!regex::evaluate(complied_complement, test_string));
      } else {
        CHECK(!regex::evaluate(compiled, test_string));
        CHECK(regex::evaluate(complied_complement, test_string));
      }
    }
  }
}

TEST_CASE(custom_grouping, "[regex][groups][custom]") {
  CHECK(does_match(R"([_0-9a-zA-Z@]+)", "_09abcABC@"));
  CHECK(!does_match(R"([_0-9a-zA-Z@]+)", "!"));
  CHECK(!does_match(R"([^_0-9a-zA-Z@]+)", "123"));


  CHECK(does_match(R"([\w]+)", "_09abcABC"));
  CHECK(!does_match(R"([\w])", "@"));
  CHECK(does_match(R"([^\w]+)", "!!!"));

  CHECK(does_match(R"([\w\s]+)", "a9b cABC"));
  CHECK(!does_match(R"([\w\s]+)", "a9b c@ABC"));
  CHECK(!does_match(R"([^\w\s]+)", "abc"));
}
