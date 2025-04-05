#pragma once

#include "private/small_set.hpp"

#include <cstddef>
#include <ostream>
#include <vector>

namespace uregex {

struct RegexGraphImpl;

class GraphAnalyzer {
  size_t m_node_count;
  std::vector<size_t> m_distance_map;
  std::vector<SmallSet<size_t>> m_non_zero_counter_map;
  std::vector<SmallSet<size_t>> m_maybe_group_map;

public:
  GraphAnalyzer(RegexGraphImpl const &graph);

  [[nodiscard]] auto distance(size_t to_node, size_t from_node) -> size_t & {
    return m_distance_map.at(from_node * m_node_count + to_node);
  }

  [[nodiscard]] auto distance(size_t to_node, size_t from_node) const
      -> size_t const & {
    return m_distance_map.at(from_node * m_node_count + to_node);
  }

  [[nodiscard]] auto are_connected(size_t to_node, size_t from_node) const
      -> bool {
    return distance(to_node, from_node) != ~0UL;
  }

  auto get_non_zero_counters(size_t at_node) const -> SmallSet<size_t> const & {
    return m_non_zero_counter_map[at_node];
  }

  auto get_maybe_set_groups(size_t at_node) const -> SmallSet<size_t> const & {
    return m_maybe_group_map[at_node];
  }

  auto visualize(std::ostream &) const -> void;
};

} // namespace uregex
