#include "regex/regex.hpp"

#include "private/jit.hpp"
#include "private/nfa.hpp"

#include <string_view>

using namespace regex;

RegexGraph::RegexGraph(std::unique_ptr<RegexGraphImpl> impl)
    : impl_{std::move(impl)} {}

RegexGraph::~RegexGraph() {};

RegexCompiled::RegexCompiled(std::unique_ptr<RegexCompiledImpl> impl)
    : impl_{std::move(impl)} {}

RegexCompiled::~RegexCompiled() {};

auto RegexCompiled::evaluate(std::string_view text) const -> MatchResult {
  return impl_->evaluate(text);
}
