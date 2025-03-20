#include "regex/regex.hpp"

#include "private/nfa.hpp"

regex::RegexGraph::RegexGraph(std::unique_ptr<RegexGraphImpl> impl)
    : impl_{std::move(impl)} {}

regex::RegexGraph::~RegexGraph() {};
