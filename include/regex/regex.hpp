#pragma once

#include <cassert>
#include <format>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace uregex {
class RegexError : public std::runtime_error {
public:
  template <typename... T>
  explicit constexpr RegexError(std::string_view fmt_string, T &&...args)
      : std::runtime_error(std::vformat(
            fmt_string, std::make_format_args(std::forward<T>(args)...))) {}
};

class ParserError : public RegexError {
public:
  using RegexError::RegexError;
};

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

struct RegexGraphImpl;
struct RegexGraph {
  std::unique_ptr<RegexGraphImpl> impl_;

  RegexGraph(std::unique_ptr<RegexGraphImpl>);

  auto visualize(std::ostream &) const -> void;
  auto evaluate(std::string_view text) const -> MatchResult;

  RegexGraph(RegexGraph &&) = default;
  auto operator=(RegexGraph &&) -> RegexGraph & = default;
  ~RegexGraph();
};

struct RegexCompiledImpl;
struct RegexCompiled {
  std::unique_ptr<RegexCompiledImpl> impl_;

  auto visualize(std::ostream &) const -> void;
  RegexCompiled(std::unique_ptr<RegexCompiledImpl>);

  auto evaluate(std::string_view text) const -> MatchResult;

  RegexCompiled(RegexCompiled &&) = default;
  auto operator=(RegexCompiled &&) -> RegexCompiled & = default;
  ~RegexCompiled();
};

auto parse(std::string_view regex_string) -> RegexGraph;
auto compile(RegexGraph &&) -> RegexCompiled;
} // namespace uregex
