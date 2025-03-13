#pragma once

#include "nfa.hpp"

#include <optional>
#include <string_view>
#include <unistd.h>
#include <vector>

namespace regex {

struct MatchResult {
  struct Group {
    size_t start_index;
    size_t end_index;
  };

  std::string_view text;
  std::vector<std::optional<Group>> groups;
  bool did_match;

  constexpr operator bool() const { return did_match; }

  constexpr auto group_text(size_t group) -> std::optional<std::string_view> {
    assert(did_match);

    auto const &match = groups[group];
    if (not match.has_value()) {
      return std::nullopt;
    }

    return text.substr(match->start_index, match->end_index);
  }
};

auto evaluate(RegexGraph &, std::string_view string) -> MatchResult;
} // namespace regex
