#include "regex/regex.hpp"

#include "private/evaluation.hpp"
#include "private/nfa.hpp"
#include "private/unicode.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string.h>
#include <vector>

using namespace uregex;
using namespace std::ranges;
using namespace uregex::evaluation;

namespace {
inline auto replace_if_better(StateAtIndex &state_to_update,
                              StateAtIndex const &current_state,
                              size_t evaluating_id, RegexGraphImpl const &graph,
                              Edge const &edge, size_t next_offset) -> void {
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
    out_counters[0] = next_offset;

    // Adjust based on edge transition
    for (size_t i = counter_offset; i < graph.counters.size(); i += 1) {
      size_t adjustment = 0;
      if (*counter_iter == i) {
        adjustment = 1;
        counter_iter++;
      }
      out_counters[i + 1] = current_counters[i + 1] + adjustment;
    }

    for (size_t group = 0; group < graph.number_of_groups; group += 1) {
      Group to_write = current_groups[group];
      if (*start_groups_iter == group) {
        to_write.start_index = next_offset;
        start_groups_iter++;
      }

      if (*end_groups_iter == group) {
        to_write.end_index = next_offset;
        end_groups_iter++;
      }
      out_groups[group] = to_write;
    }
  };

  if (out_counters[0] != next_offset) {
    // Option is newer (by construction)
    accept_new_transition(0);
    return;
  }

  // Target is already up-to-date, should we replace?
  for (size_t i = 0; i < graph.counters.size(); i += 1) {
    size_t adjustment = 0;
    if (*counter_iter == i) {
      adjustment = 1;
      counter_iter++;
    }
    size_t new_counter = current_counters[i + 1] + adjustment;

    if (out_counters[i + 1] == new_counter) {
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

EvaluationState::EvaluationState(RegexGraphImpl const &graph, bool do_init)
    : m_storage{graph.current_state.data()}, m_state_1{graph, m_storage},
      m_state_2{graph,
                m_storage + StateAtIndex::required_allocation_size(graph)} {
  if (do_init) {
    ::memcpy(m_storage, graph.initial_state.data(),
             2 * StateAtIndex::required_allocation_size(graph));
  }
}

auto EvaluationState::preallocate_initial_state(const RegexGraphImpl &graph)
    -> AlignedData {
  size_t const node_count = graph.all_nodes.size();
  size_t const counters = graph.counters.size();
  size_t const number_of_groups = graph.number_of_groups;

  size_t const allocation_size = StateAtIndex::required_allocation_size(graph);

  auto storage = AlignedData(2 * allocation_size);

  auto state_1 = StateAtIndex{graph, storage.data()};
  auto state_2 = StateAtIndex{graph, storage.data() + allocation_size};

  ::memset(state_1.groups, -1,
           StateAtIndex::group_allocation_size(node_count, number_of_groups));
  ::memset(state_1.counters, 0,
           StateAtIndex::counter_allocation_size(node_count, counters));
  ::memset(state_2.groups, -1,
           StateAtIndex::group_allocation_size(node_count, number_of_groups));
  ::memset(state_2.counters, 0,
           StateAtIndex::counter_allocation_size(node_count, counters));

  // The first counter of each state is used as the "last added index"
  for (size_t node_id = 0; node_id < node_count; node_id += 1) {
    state_1.counters_for(node_id)[0] = max_index;
  }

  for (auto const &edge : graph.entry->edges) {
    replace_if_better(state_1, state_2, graph.entry->index, graph, edge, 0);
    state_1.counters_for(edge.output_index)[0] = 0;
  }
  return storage;
}

auto EvaluationState::calculate_match_result(MatchResult &result,
                                             StateAtIndex *current_state,
                                             RegexGraphImpl const &graph,
                                             std::string_view text,
                                             size_t offset) -> bool {
  result.text = text;
  result.groups.clear();
  if (current_state->counters_for(graph.match->index)[0] !=
      text.size() + offset) {
    result.did_match = false;
    return false;
  }

  // The last character was a match!
  result.did_match = true;

  auto const *groups = current_state->groups_for(graph.match->index);
  for (size_t group_id = 0; group_id < graph.number_of_groups; group_id += 1) {
    auto const &group = groups[group_id];
    if (group.start_index != max_index && group.end_index != max_index) {
      result.groups.push_back(MatchResult::Group{group.start_index - offset,
                                                 group.end_index - offset});
    } else if (group.end_index != max_index) {
      // Zero length match
      result.groups.push_back(MatchResult::Group{group.end_index - offset,
                                                 group.end_index - offset});
    } else {
      // Unmatched group
      assert(group.start_index == max_index);
      result.groups.push_back(std::nullopt);
    }
  }
  return true;
}

auto RegexGraph::evaluate(MatchResult &result, std::string_view text) const
    -> bool {
  auto const &graph = *impl_;

  auto evaluation_state = EvaluationState{graph};

  auto *current_state = &evaluation_state.m_state_1;
  auto *next_state = &evaluation_state.m_state_2;

  // Evaluate!
  size_t current_index = 0;
  while (current_index < text.size()) {
    size_t codepoint_size = 0;
    Codepoint codepoint =
        parse_utf8_char(sub_unchecked(text, current_index), codepoint_size);

    for (size_t evaluating_id = 0; evaluating_id < graph.all_nodes.size();
         evaluating_id += 1) {
      auto const &evaluating_node = *graph.all_nodes[evaluating_id];
      if (current_state->counters_for(evaluating_id)[0] != current_index) {
        continue;
      }

      auto result = std::visit(
          [&](auto condition) {
            return evaluate_condition(codepoint, condition);
          },
          evaluating_node.condition.type);

      if (result) {
        for (auto const &edge : evaluating_node.edges) {
          replace_if_better(*next_state, *current_state, evaluating_id, graph,
                            edge, current_index + codepoint_size);
        }
      }
    }

    current_index += codepoint_size;
    std::swap(current_state, next_state);
  }
  return evaluation_state.calculate_match_result(result, current_state, graph,
                                                 text);
}
