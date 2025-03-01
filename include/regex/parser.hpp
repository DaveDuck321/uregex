#pragma once

#include "common.hpp"
#include "nfa.hpp"

#include <string_view>
#include <unistd.h>

namespace regex {
class ParserError : public RegexError {
public:
  using RegexError::RegexError;
};

auto parse(std::string_view regex_string) -> RegexGraph;
} // namespace regex
