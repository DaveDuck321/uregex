#pragma once

#include "private/character_categories.hpp"
#include "private/meta.hpp"
#include "private/small_set.hpp"

#include <cstddef>
#include <memory>
#include <vector>

namespace regex {
// A regex graph is built of states and conditions, if the condition is met we
// advance to the next state, if the condition is violated, we fail to match
// this part of the graph.
struct Condition {
  // Terminal nodes have sentinel conditions
  struct Entry {};
  struct Match {};

  // Character classes are collapsed into the parent's variant
  template <typename ClassType> struct CharacterClass {
    bool is_complement;
  };
  using CharacterClassConditions =
      meta::apply<CharacterClass, category::CharacterClassesList>;

  // Custom expressions can model both CharacterClasses and Codepoints. They are
  // separated out for performance reasons.
  struct CustomExpression {
    using CustomExpressionVariant =
        meta::rename<std::variant,
                     meta::concat<CharacterClassConditions,
                                  meta::TypeList<Codepoint, category::Range>>>;

    std::vector<CustomExpressionVariant> expressions;
    bool is_complement;
  };

  using ConditionTypeList = meta::concat<
      CharacterClassConditions,
      meta::TypeList<Entry, Match, category::Any, Codepoint, CustomExpression>>;

  using ConditionVariant = meta::rename<std::variant, ConditionTypeList>;
  ConditionVariant type;
};

struct Node;
struct Edge {
  size_t output_index;
  SmallSet<size_t> start_groups;
  SmallSet<size_t> end_groups;
  SmallSet<size_t> counters;
};

struct Node {
  size_t index;
  Condition condition;
  std::vector<Edge> edges;
};

enum class Counter {
  greedy,
  non_greedy,
};

struct RegexGraphImpl {
  std::vector<std::unique_ptr<Node>> all_nodes;
  std::vector<Counter> counters;
  std::unique_ptr<uint8_t[]> initial_state;
  Node const *entry;
  Node const *match;
  size_t number_of_groups;
};
} // namespace regex
