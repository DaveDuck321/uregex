#pragma once

#include "regex/regex.hpp"
#include "testing.hpp"

#include <ranges>
#include <string_view>

inline auto match(std::string_view regex, std::string_view string)
    -> uregex::MatchResult {
  auto graph = uregex::parse(regex);
  uregex::MatchResult cpp_evaluate_result;
  {
    bool did_match = graph.evaluate(cpp_evaluate_result, string);
    CHECK(did_match == cpp_evaluate_result);
  }

  auto compiled = uregex::compile(std::move(graph));
  uregex::MatchResult jit_evaluate_result;
  {
    bool did_match = compiled.evaluate(jit_evaluate_result, string);
    CHECK(did_match == jit_evaluate_result);
  }
  CHECK((bool)cpp_evaluate_result == (bool)jit_evaluate_result);
  CHECK(cpp_evaluate_result.groups.size() == jit_evaluate_result.groups.size());

  for (auto const &[group1, group2] : std::ranges::views::zip(
           cpp_evaluate_result.groups, jit_evaluate_result.groups)) {
    CHECK(group1.has_value() == group2.has_value());
    if (group1.has_value()) {
      CHECK(group1->start_index == group2->start_index);
      CHECK(group1->end_index == group2->end_index);
    }
  }
  return cpp_evaluate_result;
}

inline auto does_match(std::string_view regex, std::string_view string)
    -> bool {
  return match(regex, string);
}
