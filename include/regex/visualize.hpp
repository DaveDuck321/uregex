#pragma once

#include "nfa.hpp"

#include <ostream>

namespace regex {
auto output_graph(std::ostream &, const RegexGraph &) -> void;
}
