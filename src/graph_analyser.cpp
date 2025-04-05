#include "private/graph_analyser.hpp"
#include "private/nfa.hpp"

#include <cstddef>
#include <queue>

using namespace uregex;

GraphAnalyzer::GraphAnalyzer(RegexGraphImpl const &graph)
    : m_node_count{graph.all_nodes.size()},
      m_distance_map(m_node_count * m_node_count),
      m_non_zero_counter_map(m_node_count), m_maybe_group_map(m_node_count) {
  std::fill(m_distance_map.begin(), m_distance_map.end(), ~0UL);

  for (size_t start_node = 0; start_node < m_node_count; start_node += 1) {
    std::queue<std::pair<uint32_t, uint32_t>> remaining;
    remaining.emplace(1, start_node);

    while (not remaining.empty()) {
      auto [generation, current_node] = remaining.front();
      remaining.pop();

      for (auto const &edge : graph.all_nodes[current_node]->edges) {
        if (not are_connected(edge.output_index, start_node)) {
          remaining.emplace(generation + 1, edge.output_index);
          distance(edge.output_index, start_node) = generation;
        }
      }
    }
  }

  for (auto const &node : graph.all_nodes) {
    for (auto const &edge : node->edges) {
      for (size_t to_node = 0; to_node < m_node_count; to_node += 1) {
        if (not are_connected(to_node, edge.output_index) &&
            to_node != edge.output_index) {
          continue;
        }

        for (size_t counter : edge.counters) {
          m_non_zero_counter_map[to_node].insert(counter);
        }
        for (size_t group : edge.start_groups) {
          m_maybe_group_map[to_node].insert(group);
        }
        for (size_t group : edge.end_groups) {
          m_maybe_group_map[to_node].insert(group);
        }
      }
    }
  }
}

auto GraphAnalyzer::visualize(std::ostream &output) const -> void {
  for (size_t n1 = 0; n1 < m_node_count; n1 += 1) {
    for (size_t n2 = 0; n2 < m_node_count; n2 += 1) {
      auto d = distance(n1, n2);
      if (d != ~0UL) {
        output << std::format("{:<5}", d);
      } else {
        output << std::format("{:<5}", "x");
      }
    }
    output << "\n";
  }
}
