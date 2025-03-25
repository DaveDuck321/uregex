#include "regex/regex.hpp"

#include "private/jit.hpp"
#include "private/nfa.hpp"

#include <string_view>

using namespace uregex;

RegexGraph::RegexGraph(std::unique_ptr<RegexGraphImpl> impl)
    : impl_{std::move(impl)} {}

RegexGraph::~RegexGraph() {};

auto RegexGraph::evaluate(std::string_view text) const -> MatchResult {
  MatchResult result;
  evaluate(result, text);
  return result;
}

RegexCompiled::RegexCompiled(std::unique_ptr<RegexCompiledImpl> impl)
    : impl_{std::move(impl)} {}

RegexCompiled::~RegexCompiled() {};

auto RegexCompiled::evaluate(MatchResult &out, std::string_view text) const
    -> bool {
  return impl_->evaluate(out, text);
}

auto RegexCompiled::evaluate(std::string_view text) const -> MatchResult {
  MatchResult result;
  evaluate(result, text);
  return result;
}
