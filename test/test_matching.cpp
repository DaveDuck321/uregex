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

TEST_CASE(linear, "[regex][matching]") {
  CHECK(does_match(R"(...)", "abc"));
  CHECK(!does_match(R"(..)", "abc"));
  CHECK(!does_match(R"(....)", "abc"));
  CHECK(!does_match(R"(....)", "abc"));
  CHECK(does_match(R"(a(bc))", "abc"));
  CHECK(!does_match(R"(aec)", "abc"));
}

TEST_CASE(one_or_more, "[regex][matching]") {
  CHECK(does_match(R"(...+)", "abc"));
  CHECK(does_match(R"(..+)", "abc"));
  CHECK(does_match(R"(.+)", "abc"));
  CHECK(does_match(R"(.+.)", "abc"));
  CHECK(!does_match(R"(....+)", "abc"));

  CHECK(does_match(R"((.)+)", "abc"));
  CHECK(does_match(R"(.(..)+)", "abc"));
  CHECK(does_match(R"(.(((..)))+)", "abc"));
  CHECK(does_match(R"((.)+..)", "abc"));
  CHECK(!does_match(R"(.(.)+..)", "abc"));
  CHECK(!does_match(R"(a+c)", "aaabaac"));
  CHECK(does_match(R"(a+ba+c)", "aaabaac"));
}

TEST_CASE(none_or_more, "[regex][matching]") {
  CHECK(does_match(R"(...*)", "abc"));
  CHECK(does_match(R"(..*)", "abc"));
  CHECK(does_match(R"(.*)", "abc"));
  CHECK(does_match(R"(.*.)", "abc"));
  CHECK(does_match(R"(....*)", "abc"));
  CHECK(!does_match(R"(.....*)", "abc"));

  CHECK(does_match(R"((.)*)", "abc"));
  CHECK(does_match(R"(.(..)*)", "abc"));
  CHECK(does_match(R"(.(((..)))*)", "abc"));
  CHECK(does_match(R"((.)*..)", "abc"));
  CHECK(does_match(R"(.(.)*..)", "abc"));
  CHECK(does_match(R"((a*b*a*c*)*)", "aaabaac"));
}

TEST_CASE(exactly, "[regex][matching]") {
  CHECK(does_match(R"(.{3})", "abc"));
  CHECK(!does_match(R"(.{2})", "abc"));
  CHECK(!does_match(R"(.{4})", "abc"));
  CHECK(does_match(R"(((.)){3})", "abc"));
  CHECK(does_match(R"(((.{3})))", "abc"));
  CHECK(does_match(R"(ab((c{1})))", "abc"));
}

TEST_CASE(range, "[regex][matching]") {
  CHECK(!does_match(R"(.{2,4})", "a"));
  CHECK(does_match(R"(.{2,4})", "ab"));
  CHECK(does_match(R"(.{2,4})", "abc"));
  CHECK(does_match(R"(.{2,4})", "abcd"));
  CHECK(!does_match(R"(.{2,4})", "abcde"));
  CHECK(!does_match(R"(.{2,4})", "abcde"));
}

TEST_CASE(maybe, "[regex][matching]") {
  CHECK(does_match(R"(...?)", "abc"));
  CHECK(does_match(R"(....?)", "abc"));
  CHECK(does_match(R"(abcd?)", "abc"));
  CHECK(!does_match(R"(abcde?)", "abc"));
  CHECK(does_match(R"(abc?)", "abc"));
  CHECK(does_match(R"(ab(c?){50})", "abc"));
  CHECK(does_match(R"(ab(c?){50})", "ab"));
}

TEST_CASE(either, "[regex][matching]") {
  CHECK(does_match(R"(abc|abe|hij|klmnop)", "abc"));
  CHECK(does_match(R"(abc|abe|hij|klmnop)", "klmnop"));
  CHECK(!does_match(R"(abc|ace|hij|klmnop)", "abe"));
  CHECK(does_match(R"(abc|ace|hij|klmnop)", "ace"));
  CHECK(does_match(R"((a|b)*(c|d)?(f|h)+)", "abcff"));
  CHECK(does_match(R"((((a))|b)*(((c|d))?(f|((h)))+))", "abcff"));
  CHECK(!does_match(R"((a|b)*(c|d)?(f|h)+)", "abcdff"));
  CHECK(does_match(R"((a|b)*(c|d)?(f|h)+)", "aaabbafhhf"));
  CHECK(does_match(R"((a|b)*(c|d)?(f|h)+)", "f"));
  CHECK(does_match(R"((a|b)*(c|d)?(f|h)+)", "hffh"));
  CHECK(does_match(R"((a|b)*(c|d)?(f|h)+)", "bhffh"));
  CHECK(!does_match(R"((a|b)*(c|d)?(f|h)+)", ""));
  CHECK(!does_match(R"((a|b)*(c|d)?(f|h)+)", "ad"));
}
