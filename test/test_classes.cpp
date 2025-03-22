#include "regex/regex.hpp"

#include "regex_test_common.hpp"
#include "testing.hpp"

#include <cctype>
#include <cstdlib>
#include <map>
#include <string_view>
#include <sys/types.h>

using namespace std::literals;

TEST_CASE(basic_digit, "[regex][classes]") {
  CHECK(does_match(R"(\d)", "7"));
  CHECK(!does_match(R"(\d)", "a"));
  CHECK(!does_match(R"(\d)", "_"));

  CHECK(!does_match(R"(\D)", "7"));
  CHECK(does_match(R"(\D)", "a"));
  CHECK(does_match(R"(\D)", "_"));
}

TEST_CASE(basic_whitespace, "[regex][classes]") {
  CHECK(does_match(R"(\s)", "\t"));
  CHECK(does_match(R"(\s)", " "));
  CHECK(does_match(R"(\s)", "\n"));
  CHECK(does_match(R"(\s)", "\r"));
  CHECK(does_match(R"(\s)", " "));

  CHECK(!does_match(R"(\s)", "_"));
  CHECK(does_match(R"(\S)", "9"));
  CHECK(!does_match(R"(\S)", "\t"));
}

TEST_CASE(basic_alphanumeric, "[regex][classes]") {
  CHECK(does_match(R"(\w+)", "_09abcABC"));
  CHECK(!does_match(R"(\w+)", "Ā"));
  CHECK(does_match(R"(\W)", " "));
}

TEST_CASE(unicode_categories, "[regex][classes][unicode]") {
  const std::map<std::string, std::string> examples_from_category = {
      {"Cc", "\xc2\x85"},
      {"Cf", "\xe2\x81\xad"},
      {"Ll", "ꮉ"},
      {"Lm", "𖭂"},
      {"Lo", "ࠈ"},
      {"Lt", "ᾮ"},
      {"Lu", "A"},
      {"Mc", "ꦾ"},
      {"Me", "꙲"},
      {"Mn", "\xe0\xa5\x8d"},
      {"Nd", "🯷"},
      {"Nl", "Ⅻ"},
      {"No", "㈥"},
      {"Pc", "︴"},
      {"Pd", "-"},
      {"Pe", ")"},
      {"Pf", "⸡"},
      {"Pi", "“"},
      {"Po", "@"},
      {"Ps", "("},
      {"Sc", "$"},
      {"Sk", "﮶"},
      {"Sm", "⅀"},
      {"So", "⌁"},
      {"Zl", "\xe2\x80\xa8"},
      {"Zp", "\xe2\x80\xa9"},
      {"Zs", " "},
  };

  // Assert that each example matches one and ONLY one category
  for (auto const &[test_category, matching_codepoint] :
       examples_from_category) {
    auto regex = std::format("\\p{{{}}}", test_category);
    auto complement_regex = std::format("\\P{{{}}}", test_category);

    auto graph = uregex::parse(regex);
    auto complement = uregex::parse(complement_regex);

    for (auto const &[_, other_codepoint] : examples_from_category) {
      std::string test_string = other_codepoint;

      if (matching_codepoint == other_codepoint) {
        CHECK(graph.evaluate(test_string));
        CHECK(!complement.evaluate(test_string));
      } else {
        CHECK(!graph.evaluate(test_string));
        CHECK(complement.evaluate(test_string));
      }
    }
  }
}

TEST_CASE(custom_grouping, "[regex][classes][custom]") {
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
