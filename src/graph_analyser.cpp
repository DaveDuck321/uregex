#include "private/graph_analyser.hpp"
#include "private/nfa.hpp"

#include <cstdint>
#include <queue>
#include <ranges>

using namespace uregex;

struct NodeIndex {
  uint32_t value;
};
struct CounterIndex {
  uint32_t value;
};

struct StartGroupIndex {
  uint32_t value;
};

struct EndGroupIndex {
  uint32_t value;
};

GraphAnalyzer::GraphAnalyzer(RegexGraphImpl const &graph)
    : m_node_count{graph.all_nodes.size()},
      m_non_zero_counter_map(m_node_count),
      m_maybe_start_group_map(m_node_count),
      m_maybe_end_group_map(m_node_count) {
  std::queue<std::pair<NodeIndex, CounterIndex>> remaining_counters;
  std::queue<std::pair<NodeIndex, StartGroupIndex>> remaining_start_groups;
  std::queue<std::pair<NodeIndex, EndGroupIndex>> remaining_end_groups;

  for (auto const &node : graph.all_nodes) {
    for (auto const &edge : node.edges) {
      for (auto counter : edge.counters) {
        remaining_counters.emplace(edge.output_index, counter);
        m_non_zero_counter_map[edge.output_index].insert(counter);
      }
      for (auto start_group : edge.start_groups) {
        remaining_start_groups.emplace(edge.output_index, start_group);
        m_maybe_start_group_map[edge.output_index].insert(start_group);
      }
      for (auto end_group : edge.end_groups) {
        remaining_end_groups.emplace(edge.output_index, end_group);
        m_maybe_end_group_map[edge.output_index].insert(end_group);
      }
    }
  }

  while (not remaining_counters.empty()) {
    auto [node_index, counter_index] = remaining_counters.front();
    remaining_counters.pop();

    for (auto const &edge : graph.all_nodes[node_index.value].edges) {
      if (not m_non_zero_counter_map[edge.output_index].contains(
              counter_index.value)) {
        m_non_zero_counter_map[edge.output_index].insert(counter_index.value);
        remaining_counters.emplace(edge.output_index, counter_index.value);
      }
    }
  }

  while (not remaining_start_groups.empty()) {
    auto [node_index, start_group_index] = remaining_start_groups.front();
    remaining_start_groups.pop();

    for (auto const &edge : graph.all_nodes[node_index.value].edges) {
      if (not m_maybe_start_group_map[edge.output_index].contains(
              start_group_index.value)) {
        m_maybe_start_group_map[edge.output_index].insert(
            start_group_index.value);
        remaining_start_groups.emplace(edge.output_index,
                                       start_group_index.value);
      }
    }
  }

  while (not remaining_end_groups.empty()) {
    auto [node_index, end_group_index] = remaining_end_groups.front();
    remaining_end_groups.pop();

    for (auto const &edge : graph.all_nodes[node_index.value].edges) {
      if (not m_maybe_end_group_map[edge.output_index].contains(
              end_group_index.value)) {
        m_maybe_end_group_map[edge.output_index].insert(end_group_index.value);
        remaining_end_groups.emplace(edge.output_index, end_group_index.value);
      }
    }
  }
}

auto GraphAnalyzer::visualize(std::ostream &output) const -> void {
  output << "Non-zero counters:";
  for (auto const &[index, maps] :
       std::ranges::views::enumerate(std::ranges::views::zip(
           m_non_zero_counter_map, m_maybe_start_group_map,
           m_maybe_end_group_map))) {
    auto const &[counters, start_groups, end_groups] = maps;
    output << "Node[" << index << "]:\n";
    output << "Counters: ";
    for (auto const &counter : counters) {
      output << counter << ", ";
    }
    output << "\n";
    output << "Start Groups: ";
    for (auto const &group : start_groups) {
      output << group << ", ";
    }
    output << "\n";
    output << "End Groups: ";
    for (auto const &group : end_groups) {
      output << group << ", ";
    }
    output << "\n\n";
  }
}
