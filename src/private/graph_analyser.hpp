#pragma once

#include "private/small_set.hpp"

#include <cstddef>
#include <cstdint>
#include <ostream>
#include <vector>

namespace uregex {

struct RegexGraphImpl;

class GraphAnalyzer {
  size_t m_node_count;
  std::vector<SmallSet<uint32_t>> m_non_zero_counter_map;
  std::vector<SmallSet<uint32_t>> m_maybe_start_group_map;
  std::vector<SmallSet<uint32_t>> m_maybe_end_group_map;

public:
  GraphAnalyzer(RegexGraphImpl const &graph);

  [[nodiscard]] constexpr auto
  get_non_zero_counters(size_t at_node) const -> SmallSet<uint32_t> const & {
    return m_non_zero_counter_map[at_node];
  }

  [[nodiscard]] constexpr auto get_maybe_set_start_groups(size_t at_node) const
      -> SmallSet<uint32_t> const & {
    return m_maybe_start_group_map[at_node];
  }

  [[nodiscard]] constexpr auto
  get_maybe_set_end_groups(size_t at_node) const -> SmallSet<uint32_t> const & {
    return m_maybe_end_group_map[at_node];
  }

  [[nodiscard]] constexpr auto
  is_group_start_unset(size_t at_node, size_t group) const -> bool {
    return !get_maybe_set_start_groups(at_node).contains(group);
  }

  [[nodiscard]] constexpr auto is_group_end_unset(size_t at_node,
                                                  size_t group) const -> bool {
    return !get_maybe_set_end_groups(at_node).contains(group);
  }

  [[nodiscard]] constexpr auto is_counter_zero(size_t at_node,
                                               size_t counter) const -> bool {
    return !get_non_zero_counters(at_node).contains(counter);
  }

  auto visualize(std::ostream &) const -> void;
};

} // namespace uregex
