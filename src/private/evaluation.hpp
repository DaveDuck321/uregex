#pragma once

#include "private/nfa.hpp"
#include "private/unicode.hpp"
#include "regex/regex.hpp"

#include <cstddef>
#include <cstdint>
#include <string.h>
#include <string_view>
#include <variant>

namespace uregex::evaluation {
using CounterType = unsigned;
using IndexType = unsigned;
static constexpr auto max_index = std::numeric_limits<IndexType>::max();

struct Group {
  IndexType start_index;
  IndexType end_index;
};
struct StateAtIndex {
  Group *groups;
  CounterType *counters;

  size_t counter_stride;
  size_t group_stride;

  StateAtIndex(uint8_t storage[], size_t node_count, size_t counter_count,
               size_t group_count)
      : groups{reinterpret_cast<Group *>(
            &storage[counter_allocation_size(node_count, counter_count)])},
        counters{reinterpret_cast<CounterType *>(&storage[0])},
        counter_stride{counter_count + 1}, group_stride{group_count} {}

  StateAtIndex(RegexGraphImpl const &graph, uint8_t storage[])
      : StateAtIndex{storage, graph.all_nodes.size(), graph.counters.size(),
                     graph.number_of_groups} {}

  static constexpr auto counter_allocation_size(size_t node_count,
                                                size_t counter_count)
      -> size_t {
    return sizeof(CounterType) * (counter_count + 1) * node_count;
  }

  static constexpr auto counter_offset(RegexGraphImpl const &) -> size_t {
    return 0;
  }

  static constexpr auto group_allocation_size(size_t node_count,
                                              size_t group_count) -> size_t {
    return sizeof(Group) * group_count * node_count;
  }

  static constexpr auto group_offset(RegexGraphImpl const &graph) -> size_t {
    return counter_allocation_size(graph.all_nodes.size(),
                                   graph.counters.size());
  }

  static constexpr auto required_allocation_size(size_t node_count,
                                                 size_t counter_count,
                                                 size_t group_count) -> size_t {
    size_t unaligned_size = group_allocation_size(node_count, group_count) +
                            counter_allocation_size(node_count, counter_count);
    return (unaligned_size + sizeof(size_t) - 1) & ~(sizeof(size_t) - 1);
  }

  static constexpr auto required_allocation_size(RegexGraphImpl const &graph)
      -> size_t {
    return required_allocation_size(
        graph.all_nodes.size(), graph.counters.size(), graph.number_of_groups);
  }

  constexpr auto counters_for(size_t state_id) const -> CounterType * {
    return &counters[state_id * counter_stride];
  }
  constexpr auto groups_for(size_t state_id) const -> Group * {
    return &groups[state_id * group_stride];
  }
};

constexpr auto evaluate_condition(Codepoint, Condition::Entry const &) -> bool {
  return false; // Unreachable
}

constexpr auto evaluate_condition(Codepoint, Condition::Match const &) -> bool {
  return false; // An early match, just discard it
}

constexpr auto evaluate_condition(Codepoint, category::Any const &) -> bool {
  return true;
}

constexpr auto evaluate_condition(Codepoint codepoint, Codepoint const &target)
    -> bool {
  return codepoint == target;
}

constexpr auto evaluate_condition(Codepoint codepoint,
                                  category::Range const &range) -> bool {
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
                   Condition::CharacterClass<Class> const &character_class)
    -> bool {
  return character_class.is_complement ^ is_in_category<Class>(codepoint);
}

constexpr auto evaluate_condition(Codepoint codepoint,
                                  Condition::CustomExpression const &expression)
    -> bool {
  for (auto const &item : expression.expressions) {
    auto result = std::visit(
        [&](auto const &condition) {
          return evaluate_condition(codepoint, condition);
        },
        item);
    if (result) {
      return !expression.is_complement;
    }
  }
  return expression.is_complement;
}

constexpr auto are_mutually_exclusive(Condition condition1,
                                      Condition condition2) -> bool {

  if (std::holds_alternative<Condition::Match>(condition1.type) ||
      std::holds_alternative<Condition::Entry>(condition1.type) ||
      std::holds_alternative<Condition::Match>(condition2.type) ||
      std::holds_alternative<Condition::Entry>(condition2.type)) {
    // Placeholder states are never even evaluated
    return true;
  }

  if (std::holds_alternative<Codepoint>(condition2.type)) {
    std::swap(condition1, condition1);
  }

  if (std::holds_alternative<Codepoint>(condition1.type)) {
    return !std::visit(
        [&](auto c2) {
          return evaluate_condition(std::get<Codepoint>(condition1.type), c2);
        },
        condition2.type);
  }

  return false;
}

struct EvaluationState {
  uint8_t *m_storage;

  StateAtIndex m_state_1;
  StateAtIndex m_state_2;

  explicit EvaluationState(RegexGraphImpl const &, bool do_init = true);

  static auto preallocate_initial_state(RegexGraphImpl const &) -> AlignedData;
  auto calculate_match_result(MatchResult &, StateAtIndex *current_state,
                              RegexGraphImpl const &, std::string_view text,
                              size_t offset = 0) -> bool;
};

constexpr auto sub_unchecked(std::string_view view, size_t start_index)
    -> std::string_view {
  return {view.data() + start_index, view.size() - start_index};
}
} // namespace uregex::evaluation
