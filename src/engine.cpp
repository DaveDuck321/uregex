#include "regex/engine.hpp"
#include "regex/character_categories.hpp"
#include "regex/nfa.hpp"
#include "regex/unicode.hpp"

#include <cassert>

using namespace regex;

namespace {
constexpr auto evaluate_condition(Codepoint, Condition::Entry) -> bool {
  return false; // Unreachable
}

constexpr auto evaluate_condition(Codepoint, Condition::Match) -> bool {
  return false; // An early match, just discard it
}

constexpr auto evaluate_condition(Codepoint, category::Any) -> bool {
  return true;
}

constexpr auto evaluate_condition(Codepoint codepoint, Codepoint target)
    -> bool {
  return codepoint == target;
}

constexpr auto evaluate_condition(Codepoint codepoint, category::Range range)
    -> bool {
  return range.lower.value <= codepoint.value &&
         range.upper.value >= codepoint.value;
}

template <typename Class>
constexpr auto
evaluate_condition(Codepoint,
                   Condition::CharacterClass<Class> character_class) -> bool {
  return !character_class.is_complement; // TODO
}

constexpr auto evaluate_condition(Codepoint codepoint,
                                  Condition::CustomExpression expression)
    -> bool {
  for (auto const &item : expression.expressions) {
    auto result = std::visit(
        [&](auto condition) {
          return evaluate_condition(codepoint, condition);
        },
        item);
    if (result) {
      return true;
    }
  }
  return false;
}
} // namespace

auto regex::evaluate(RegexGraph &graph, std::string_view string) -> bool {
  std::vector<State *> evaluating = {graph.entry->output_states.begin(),
                                     graph.entry->output_states.end()};
  std::vector<State *> next_to_evaluate;

  size_t current_index = 0;
  for (; current_index < string.size() && evaluating.size() > 0;
       current_index += 1) {

    while (evaluating.size() > 0) {
      auto *next_state = evaluating.back();
      evaluating.pop_back();
      auto result = std::visit(
          [&](auto condition) {
            return evaluate_condition(Codepoint(string[current_index]),
                                      condition);
          },
          next_state->condition.type);

      if (result) {
        for (auto *state : next_state->output_states) {
          if (state->last_added_at_index == current_index) {
            continue; // Already added
          }
          state->last_added_at_index = current_index;
          next_to_evaluate.push_back(state);
        }
      }
    }

    std::swap(evaluating, next_to_evaluate);
    next_to_evaluate.clear();
  }

  if (current_index == string.size() &&
      graph.match->last_added_at_index == current_index - 1) {
    // The last character was a match!
    return true;
  }
  return false;
}
