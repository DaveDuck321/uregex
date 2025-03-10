#include "regex/engine.hpp"
#include "regex/character_categories.hpp"
#include "regex/nfa.hpp"
#include "regex/unicode.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <ranges>
#include <type_traits>
#include <vector>

using namespace regex;
using namespace std::ranges;

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

struct Group {
  size_t start_index = ~0U;
  size_t end_index = ~0U;
};
struct State {
  Node const *node;

  size_t last_added;
  std::vector<size_t> counts;
  std::vector<Group> groups;
};

constexpr auto edge_to_state(State const &current_state, Edge const &edge,
                             size_t index) -> State {
  State result = current_state;
  for (auto counter : edge.counters) {
    result.counts[counter] += 1;
  }
  for (auto group : edge.start_groups) {
    result.groups[group].start_index = index;
  }
  for (auto group : edge.end_groups) {
    result.groups[group].end_index = index;
  }
  result.last_added = index;
  result.node = edge.output;
  return result;
}

constexpr auto replace_if_better(std::vector<Counter> const &all_counters,
                                 State &target, State &&option) -> bool {
  if (target.last_added != option.last_added) {
    // Option is newer (by construction)
    target = std::move(option);
    return true;
  }

  // Target is already up-to-date, should we replace?
  for (auto [type, option_count, existing_count] :
       views::zip(all_counters, option.counts, target.counts)) {
    if (option_count == existing_count) {
      continue;
    }

    if ((type == Counter::non_greedy) ^ (option_count > existing_count)) {
      target = std::move(option);
    }
    break;
  }
  return false;
}
} // namespace

auto regex::evaluate(RegexGraph &graph, std::string_view string)
    -> MatchResult {
  std::vector<size_t> evaluating;
  std::vector<size_t> next_to_evaluate;

  // Allocate states and setup the first evaluation
  std::vector<State> states;
  states.reserve(graph.all_nodes.size());

  for (auto &node : graph.all_nodes) {
    State state = {
        .node = node.get(),
        .last_added = ~0U,
        .counts = std::vector<size_t>(graph.counters.size()),
        .groups = std::vector<Group>(graph.number_of_groups),
    };
    states.push_back(std::move(state));
  }
  std::vector<State> next_states = states;

  for (auto const &edge : graph.entry->edges) {
    auto state = edge_to_state(states[graph.entry->index], edge, 0);
    bool is_new = replace_if_better(graph.counters, states[edge.output->index],
                                    std::move(state));
    if (is_new) {
      evaluating.push_back(edge.output->index);
    }
  }

  // Evaluate!
  size_t current_index = 0;
  while (current_index < string.size() && evaluating.size() > 0) {
    size_t codepoint_size = 0;
    Codepoint codepoint =
        parse_utf8_char(string.substr(current_index), codepoint_size);
    current_index += codepoint_size;

    while (evaluating.size() > 0) {
      auto &evaluating_state = states[evaluating.back()];
      auto const *evaluating_node = evaluating_state.node;

      evaluating.pop_back();
      auto result = std::visit(
          [&](auto condition) {
            return evaluate_condition(codepoint, condition);
          },
          evaluating_node->condition.type);

      if (result) {
        for (auto const &edge : evaluating_node->edges) {
          auto proposed = edge_to_state(evaluating_state, edge, current_index);

          bool is_new =
              replace_if_better(graph.counters, next_states[edge.output->index],
                                std::move(proposed));
          if (is_new) {
            next_to_evaluate.push_back(edge.output->index);
          }
        }
      }
    }

    std::swap(evaluating, next_to_evaluate);
    std::swap(next_states, states);
    next_to_evaluate.clear();
  }

  if (current_index == string.size() &&
      states[graph.match->index].last_added == current_index) {
    // The last character was a match!
    auto result = MatchResult{{}, true};
    result.groups.reserve(graph.number_of_groups);
    for (const auto &group : states[graph.match->index].groups) {
      if (group.start_index != ~0U && group.end_index != ~0U) {
        result.groups.push_back(
            MatchResult::Group{group.start_index, group.end_index});
      } else if (group.end_index != ~0U) {
        // Zero length match
        result.groups.push_back(
            MatchResult::Group{group.end_index, group.end_index});
      } else {
        // Unmatched group
        assert(group.start_index == ~0U);
        result.groups.push_back({});
      }
    }
    return result;
  }
  return MatchResult{{}, false};
}
