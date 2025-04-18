#pragma once

#include "private/allocation.hpp"
#include "private/character_categories.hpp"
#include "private/meta.hpp"
#include "private/small_set.hpp"
#include "private/small_vector.hpp"

#include <cstddef>
#include <memory>
#include <sys/mman.h>
#include <vector>

namespace uregex {
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
  struct CustomExpressionImpl {
    using CustomExpressionVariant =
        meta::rename<std::variant,
                     meta::concat<CharacterClassConditions,
                                  meta::TypeList<Codepoint, category::Range>>>;

    std::vector<CustomExpressionVariant> expressions;
    bool is_complement;
  };

  // CustomExpressionImpl largest member of ConditionVariant. Store it on the
  // heap to avoid `Condition` getting too large.
  using CustomExpression = std::unique_ptr<CustomExpressionImpl>;

  using ConditionTypeList = meta::concat<
      CharacterClassConditions,
      meta::TypeList<Entry, Match, category::Any, Codepoint, CustomExpression>>;

  using ConditionVariant = meta::rename<std::variant, ConditionTypeList>;
  ConditionVariant type;
};

struct Node;
struct Edge {
  size_t output_index;
  SmallSet<uint32_t> start_groups;
  SmallSet<uint32_t> end_groups;
  SmallSet<uint32_t> counters;
};

struct Node {
  SmallVector<Edge, 1> edges;
};

enum class Counter {
  greedy,
  non_greedy,
};

struct RegexGraphImpl {
  std::vector<Node> all_nodes;
  std::vector<Condition> all_conditions;
  std::vector<Counter> counters;
  AlignedData initial_state;
  AlignedData current_state;
  size_t entry_node;
  size_t match_node;
  size_t number_of_groups;
};
} // namespace uregex
