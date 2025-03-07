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

  std::vector<std::optional<Group>> groups;
  bool did_match;

  operator bool() const { return did_match; }
};

auto evaluate(RegexGraph &, std::string_view string) -> MatchResult;
} // namespace regex
