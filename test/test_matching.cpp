#include "regex_test_common.hpp"
#include "testing.hpp"

#include <cctype>
#include <cstdlib>
#include <sys/types.h>

using namespace std::literals;

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

TEST_CASE(simple_string, "[regex][matching]") {
  // This is worth testing because the jit will try very hard to short-circuit
  // these compares so quite a lot can go wrong here.
  CHECK(does_match("aaa", "aaa"));
  CHECK(!does_match("aaa", "aaaa"));
  CHECK(!does_match("aaa", "aab"));

  CHECK(does_match("aaaa", "aaaa"));
  CHECK(!does_match("aaaa", "aaaaa"));
  CHECK(!does_match("aaaa", "aaab"));

  CHECK(does_match("aaaaaaaa", "aaaaaaaa"));
  CHECK(!does_match("aaaaaaaa", "aaaaaaaaa"));
  CHECK(!does_match("aaaaaaaa", "aaaaaaab"));

  CHECK(does_match("aaaaaaaaa", "aaaaaaaaa"));
  CHECK(!does_match("aaaaaaaaa", "aaaaaaaaaa"));
  CHECK(!does_match("aaaaaaaaa", "aaaaaaaab"));
}
