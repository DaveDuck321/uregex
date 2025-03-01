#pragma once

#include "nfa.hpp"

#include <string_view>
#include <unistd.h>

namespace regex {
auto evaluate(RegexGraph&, std::string_view string) -> bool;
} // namespace regex
