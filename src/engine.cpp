#include "regex/engine.hpp"
#include "regex/character_categories.hpp"
#include "regex/nfa.hpp"
#include "regex/unicode.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <limits>
#include <memory>
#include <optional>
#include <string.h>
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

using CounterType = unsigned;
using IndexType = unsigned;
static constexpr auto max_index = std::numeric_limits<IndexType>::max();

struct Group {
  IndexType start_index;
  IndexType end_index;
};
struct EvaluationState {
  std::unique_ptr<IndexType[]> last_added;
  std::unique_ptr<CounterType[]> counts;
  std::unique_ptr<Group[]> groups;

  size_t counter_count;
  size_t group_count;

  EvaluationState(size_t node_count, size_t counter_count, size_t group_count)
      : last_added{std::make_unique<IndexType[]>(node_count)},
        counts{std::make_unique<CounterType[]>(node_count * counter_count)},
        groups{std::make_unique<Group[]>(node_count * group_count)},
        counter_count{counter_count}, group_count{group_count} {
    ::memset(last_added.get(), -1, node_count * sizeof(IndexType));
    ::memset(groups.get(), -1, node_count * group_count * sizeof(Group));
  }

  constexpr auto counters_for(size_t state_id) const -> CounterType * {
    return &counts[state_id * counter_count];
  }
  constexpr auto groups_for(size_t state_id) const -> Group * {
    return &groups[state_id * group_count];
  }
};

inline auto replace_if_better(EvaluationState &state_to_update,
                              EvaluationState const &current_state,
                              size_t evaluating_id, RegexGraph const &graph,
                              Edge const &edge, size_t source_offset) -> void {
  size_t const counter_count = state_to_update.counter_count;
  size_t const group_count = state_to_update.group_count;

  size_t const node_index = edge.output_index;
  CounterType *out_counters = state_to_update.counters_for(node_index);
  auto *out_groups = state_to_update.groups_for(node_index);

  CounterType const *current_counters =
      current_state.counters_for(evaluating_id);
  auto const *current_groups = current_state.groups_for(evaluating_id);

  auto counter_iter = edge.counters.begin();

  auto accept_new_transition = [&](size_t counter_offset) {
    // Copy over matching state
    auto start_groups_iter = edge.start_groups.begin();
    auto end_groups_iter = edge.end_groups.begin();
    state_to_update.last_added[node_index] = source_offset;

    // Adjust based on edge transition
    for (size_t i = counter_offset; i < counter_count; i += 1) {
      size_t adjustment = 0;
      if (*counter_iter == i) {
        adjustment = 1;
        counter_iter++;
      }
      out_counters[i] = current_counters[i] + adjustment;
    }

    for (size_t group = 0; group < group_count; group += 1) {
      Group to_write = current_groups[group];
      if (*start_groups_iter == group) {
        to_write.start_index = source_offset;
        start_groups_iter++;
      }

      if (*end_groups_iter == group) {
        to_write.end_index = source_offset;
        end_groups_iter++;
      }
      out_groups[group] = to_write;
    }
  };

  if (state_to_update.last_added[node_index] != source_offset) {
    // Option is newer (by construction)
    accept_new_transition(0);
    return;
  }

  // Target is already up-to-date, should we replace?
  for (size_t i = 0; i < counter_count; i += 1) {
    size_t adjustment = 0;
    if (*counter_iter == i) {
      adjustment = 1;
      counter_iter++;
    }
    size_t new_counter = current_counters[i] + adjustment;

    if (out_counters[i] == new_counter) {
      continue;
    }

    auto const type = graph.counters[i];
    if ((type == Counter::non_greedy) ^ (new_counter > out_counters[i])) {
      accept_new_transition(i);
    }
    return;
  }
}
} // namespace

auto regex::evaluate(RegexGraph &graph, std::string_view string)
    -> MatchResult {
  size_t const node_count = graph.all_nodes.size();
  size_t const counter_count = graph.counters.size();
  size_t const group_count = graph.number_of_groups;

  // Work queues
  auto evaluating = std::make_unique<bool[]>(node_count);
  auto next_to_evaluate = std::make_unique<bool[]>(node_count);

  // Computation state x2 (swapped each generation)
  auto state_1 = EvaluationState{node_count, counter_count, group_count};
  auto state_2 = EvaluationState{node_count, counter_count, group_count};
  auto *current_state = &state_1;
  auto *next_state = &state_2;

  for (auto const &edge : graph.entry->edges) {
    replace_if_better(*next_state, *current_state, graph.entry->index, graph,
                      edge, 0);
    next_to_evaluate[edge.output_index] = true;
  }

  std::swap(evaluating, next_to_evaluate);
  std::swap(current_state, next_state);

  // Evaluate!
  size_t current_index = 0;
  while (current_index < string.size()) {
    size_t codepoint_size = 0;
    Codepoint codepoint =
        parse_utf8_char(string.substr(current_index), codepoint_size);
    current_index += codepoint_size;

    for (size_t evaluating_id = 0; evaluating_id < node_count;
         evaluating_id += 1) {
      auto const &evaluating_node = *graph.all_nodes[evaluating_id];
      if (not evaluating[evaluating_id]) {
        continue;
      }

      evaluating[evaluating_id] = false;

      auto result = std::visit(
          [&](auto condition) {
            return evaluate_condition(codepoint, condition);
          },
          evaluating_node.condition.type);

      if (result) {
        for (auto const &edge : evaluating_node.edges) {
          replace_if_better(*next_state, *current_state, evaluating_id, graph,
                            edge, current_index);
          next_to_evaluate[edge.output_index] = true;
        }
      }
    }

    std::swap(evaluating, next_to_evaluate);
    std::swap(current_state, next_state);
  }

  if (current_index == string.size() &&
      current_state->last_added[graph.match->index] == current_index) {
    // The last character was a match!
    auto result = MatchResult{string, {}, true};
    result.groups.reserve(graph.number_of_groups);

    auto const *groups = current_state->groups_for(graph.match->index);
    for (size_t group_id = 0; group_id < group_count; group_id += 1) {
      auto const &group = groups[group_id];
      if (group.start_index != max_index && group.end_index != max_index) {
        result.groups.push_back(
            MatchResult::Group{group.start_index, group.end_index});
      } else if (group.end_index != max_index) {
        // Zero length match
        result.groups.push_back(
            MatchResult::Group{group.end_index, group.end_index});
      } else {
        // Unmatched group
        assert(group.start_index == max_index);
        result.groups.push_back(std::nullopt);
      }
    }
    return result;
  }
  return MatchResult{string, {}, false};
}
