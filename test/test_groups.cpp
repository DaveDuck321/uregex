#include "regex/engine.hpp"
#include "regex/parser.hpp"
#include "testing.hpp"

#include <cctype>
#include <cstdlib>
#include <sys/types.h>

using namespace std::literals;

namespace {
auto match(std::string_view regex, std::string_view string)
    -> regex::MatchResult {
  auto compiled = regex::parse(regex);
  return regex::evaluate(compiled, string);
}
} // namespace

TEST_CASE(single_group, "[regex][groups]") {
  auto result = match(R"((.))", "a");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 1);
}

TEST_CASE(multi_group, "[regex][groups]") {
  auto result = match(R"((..)(.))", "abc");
  CHECK(result);

  REQUIRE(result.groups.size() == 2);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 2);

  REQUIRE(result.groups[1].has_value());
  CHECK(result.groups[1]->start_index == 2);
  CHECK(result.groups[1]->end_index == 3);
}

TEST_CASE(repeat_group, "[regex][groups]") {
  auto result = match(R"((..)+)", "abcdef");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 4);
  CHECK(result.groups[0]->end_index == 6);
}

TEST_CASE(missing_group, "[regex][groups]") {
  auto result = match(R"(abc|(d))", "abc");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  CHECK(not result.groups[0].has_value());
}

TEST_CASE(optional_group, "[regex][groups]") {
  auto result = match(R"(abc|(d))", "d");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 1);
}

TEST_CASE(greedy_match_plus, "[regex][groups]") {
  auto result = match(R"((.+)a(.+))", "aaaaa");
  CHECK(result);
  REQUIRE(result.groups.size() == 2);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 3);

  REQUIRE(result.groups[1].has_value());
  CHECK(result.groups[1]->start_index == 4);
  CHECK(result.groups[1]->end_index == 5);
}

TEST_CASE(greedy_match_asterisk, "[regex][groups]") {
  auto result = match(R"((.*)a(.*))", "aaaaa");
  CHECK(result);
  REQUIRE(result.groups.size() == 2);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 4);

  REQUIRE(result.groups[1].has_value());
  CHECK(result.groups[1]->start_index == 5);
  CHECK(result.groups[1]->end_index == 5);
}

TEST_CASE(overlapping_groups, "[regex][groups]") {
  auto result = match(R"(((a)|b)*)", "ab");
  CHECK(result);
  REQUIRE(result.groups.size() == 2);

  // Group[0].start < Group[1].start since Group[0] gets replaced by the second
  // matched character
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 1);
  CHECK(result.groups[0]->end_index == 2);

  REQUIRE(result.groups[1].has_value());
  CHECK(result.groups[1]->start_index == 0);
  CHECK(result.groups[1]->end_index == 1);
}

TEST_CASE(lazy_match_plus, "[regex][groups]") {
  auto result = match(R"((.+?)a(.+?))", "aaaaa");
  CHECK(result);
  REQUIRE(result.groups.size() == 2);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 1);

  REQUIRE(result.groups[1].has_value());
  CHECK(result.groups[1]->start_index == 2);
  CHECK(result.groups[1]->end_index == 5);
}

TEST_CASE(lazy_match_question_mark, "[regex][groups]") {
  auto result = match(R"((a??)a+)", "aa");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 0);
}

TEST_CASE(greedy_match_question_mark, "[regex][groups]") {
  auto result = match(R"((a?)a+)", "aa");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 1);
}


TEST_CASE(lazy_match_range, "[regex][groups]") {
  auto result = match(R"((a{1,6}?)a+)", "aaaaaa");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 1);
}

TEST_CASE(greedy_match_range, "[regex][groups]") {
  auto result = match(R"((a{1,6})a+)", "aaaaaa");
  CHECK(result);
  REQUIRE(result.groups.size() == 1);
  REQUIRE(result.groups[0].has_value());
  CHECK(result.groups[0]->start_index == 0);
  CHECK(result.groups[0]->end_index == 5);
}
