#pragma once

#include "regex/regex.hpp"
#include "testing.hpp"

#include <ranges>
#include <string_view>

inline auto match(std::string_view regex, std::string_view string)
    -> uregex::MatchResult {
  auto graph = uregex::parse(regex);
  auto const cpp_evaluate_result = graph.evaluate(string);

  auto compiled = uregex::compile(std::move(graph));
  auto const jit_evaluate_result = compiled.evaluate(string);
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
