#include "regex/engine.hpp"
#include "regex/character_categories.hpp"
#include "regex/nfa.hpp"
#include "regex/unicode.hpp"

#include <array>
#include <cassert>
#include <type_traits>

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

template <typename Category>
constexpr auto is_in_category(Codepoint codepoint) -> bool {
  auto input_category = get_category(codepoint);

  static_assert(std::is_base_of_v<category::Unicode, Category>);
  constexpr auto target_category = Category::category;
  if constexpr (target_category.size() == 1) {
    return input_category[0] == target_category[0];
  } else {
    static_assert(target_category.size() == 2);
    return input_category[0] == target_category[0] &&
           input_category[1] == target_category[1];
  }
}

template <>
constexpr auto is_in_category<category::ASCIIDigit>(Codepoint codepoint)
    -> bool {
  return evaluate_condition(codepoint, category::Range{'0', '9'});
}

template <>
constexpr auto is_in_category<category::ASCIIWhitespace>(Codepoint codepoint)
    -> bool {
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions/Character_classes
  switch (codepoint.value) {
  case ' ':
  case '\f':
  case '\n':
  case '\r':
  case '\t':
  case '\v':
  case 0x00a0U:
  case 0x1680U:
  case 0x2028U:
  case 0x2029U:
  case 0x202fU:
  case 0x205fU:
  case 0x3000U:
  case 0xfeffU:
    return true;
  default:
    return evaluate_condition(codepoint, category::Range{0x2000U, 0x200aU});
  }
}

template <>
constexpr auto is_in_category<category::ASCIIAlphaNumeric>(Codepoint codepoint)
    -> bool {
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions/Character_classes
  return codepoint == Codepoint{'_'} ||
         evaluate_condition(codepoint, category::Range('A', 'Z')) ||
         evaluate_condition(codepoint, category::Range('a', 'z')) ||
         is_in_category<category::ASCIIDigit>(codepoint);
}

template <typename Class>
constexpr auto
evaluate_condition(Codepoint codepoint,
                   Condition::CharacterClass<Class> character_class) -> bool {
  return character_class.is_complement ^ is_in_category<Class>(codepoint);
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
      return !expression.is_complement;
    }
  }
  return expression.is_complement;
}
} // namespace

auto regex::evaluate(RegexGraph &graph, std::string_view string) -> bool {
  std::vector<State *> evaluating = {graph.entry->output_states.begin(),
                                     graph.entry->output_states.end()};
  std::vector<State *> next_to_evaluate;

  size_t current_index = 0;
  while (current_index < string.size() && evaluating.size() > 0) {
    size_t codepoint_size = 0;
    Codepoint codepoint =
        parse_utf8_char(string.substr(current_index), codepoint_size);
    current_index += codepoint_size;

    while (evaluating.size() > 0) {
      auto *next_state = evaluating.back();
      evaluating.pop_back();
      auto result = std::visit(
          [&](auto condition) {
            return evaluate_condition(codepoint, condition);
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
      graph.match->last_added_at_index == current_index) {
    // The last character was a match!
    return true;
  }
  return false;
}
