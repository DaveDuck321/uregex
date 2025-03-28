#include "regex/regex.hpp"

#include "private/assembler.hpp"
#include "private/evaluation.hpp"
#include "private/jit.hpp"
#include "private/nfa.hpp"
#include "private/unicode.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <ranges>
#include <string.h>
#include <variant>

using namespace uregex;
using namespace uregex::jit;
using namespace uregex::evaluation;
using namespace std::ranges;

namespace {

constexpr auto current_state_base_reg = CallingConvention::callee_saved[0];
constexpr auto next_state_base_reg = CallingConvention::callee_saved[1];

constexpr auto indicies = CallingConvention::callee_saved[2];
constexpr auto codepoint = CallingConvention::callee_saved[3];

constexpr auto did_accept_any_state = CallingConvention::callee_saved[4];
constexpr auto lookahead_buffer = CallingConvention::callee_saved[5];

constexpr auto current_index = CallingConvention::temporary[1];

auto compile_commit_new_state(
    FunctionBuilder &builder, RegexGraphImpl const &graph, Edge const &edge,
    size_t current_state, std::vector<Label> const &commit_from_counter_labels)
    -> void {
  static constexpr auto computed_counter_value =
      CallingConvention::temporary[0];

  // 1) Commit new counters
  // - Commit the current index
  builder.insert_store32(next_state_base_reg,
                         /*offset=*/StateAtIndex::counter_offset(graph) +
                             sizeof(CounterType) * (graph.counters.size() + 1) *
                                 edge.output_index,
                         /*src=*/current_index);

  // - Commit each counter
  for (size_t counter_index = 0; counter_index < graph.counters.size();
       counter_index += 1) {
    size_t const current_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) *
            ((graph.counters.size() + 1) * current_state + counter_index + 1);

    size_t const next_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) * ((graph.counters.size() + 1) * edge.output_index +
                               counter_index + 1);

    builder.attach_label(commit_from_counter_labels[counter_index]);
    bool is_incremented = edge.counters.contains(counter_index);

    // TODO: maybe just issue an add with a memory operand?
    builder.insert_load32(computed_counter_value, current_state_base_reg,
                          current_state_counter_offset);

    if (is_incremented) {
      builder.insert_add(computed_counter_value, 1);
    }

    builder.insert_store32(/*dst_base=*/next_state_base_reg,
                           /*dst_offset=*/next_state_counter_offset,
                           /*src=*/computed_counter_value);
  }

  // 2) Commit new groups
  static constexpr auto tmp_group = CallingConvention::temporary[0];

  for (size_t group_index = 0; group_index < graph.number_of_groups;
       group_index += 1) {
    size_t const current_state_group_offset =
        StateAtIndex::group_offset(graph) +
        sizeof(Group) * (graph.number_of_groups * current_state + group_index);
    size_t const next_state_group_offset =
        StateAtIndex::group_offset(graph) +
        sizeof(Group) *
            (graph.number_of_groups * edge.output_index + group_index);

    if (not edge.start_groups.contains(group_index) &&
        not edge.end_groups.contains(group_index)) {
      // Common case where we're not touching this group, copy over the entire
      // struct atomically.
      builder.insert_load64(tmp_group, current_state_base_reg,
                            current_state_group_offset);
      builder.insert_store64(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset,
                             /*src=*/tmp_group);
      continue;
    }

    if (edge.start_groups.contains(group_index)) {
      builder.insert_store32(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset,
                             /*src=*/current_index);
    } else {
      builder.insert_load32(tmp_group, current_state_base_reg,
                            current_state_group_offset);
      builder.insert_store32(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset,
                             /*src=*/tmp_group);
    }

    if (edge.end_groups.contains(group_index)) {
      builder.insert_store32(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset +
                                 sizeof(Group::start_index),
                             /*src=*/current_index);
    } else {
      builder.insert_load32(tmp_group, current_state_base_reg,
                            current_state_group_offset +
                                sizeof(Group::start_index));
      builder.insert_store32(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset +
                                 sizeof(Group::start_index),
                             /*src=*/tmp_group);
    }
  }
}

auto compile_edge_transition(FunctionBuilder &builder,
                             RegexGraphImpl const &graph, Edge const &edge,
                             size_t current_state) -> void {
  static constexpr auto computed_counter_value =
      CallingConvention::temporary[0];

  // We can skip over the first N-counters if we've already incidentally proved
  // that they're unchanged. Allocate the labels ahead of time.
  std::vector<Label> commit_from_counter_labels;
  commit_from_counter_labels.reserve(graph.counters.size());
  for (size_t counter_index = 0; counter_index < graph.counters.size();
       counter_index += 1) {
    commit_from_counter_labels.push_back(builder.allocate_label());
  }

  // Special case: we can skip all the counter compares if we know that there's
  // only one incoming edge (we have already checked the actual node condition).
  if (graph.all_nodes[edge.output_index]->incoming_edges == 1) {
    compile_commit_new_state(builder, graph, edge, current_state,
                             commit_from_counter_labels);
    return;
  }

  auto const commit_new_state = builder.allocate_label();
  auto const abandon_transition = builder.allocate_label();

  // - The first counter is set to the current index if we've already
  // added the state at this iteration.
  builder.insert_load_cmp32(
      next_state_base_reg,
      /*offset=*/StateAtIndex::counter_offset(graph) +
          sizeof(CounterType) * (graph.counters.size() + 1) * edge.output_index,
      /*compare_to=*/current_index);
  builder.insert_jump_if_not_zero_flag(commit_new_state);

  // Now we need to consider the remaining counters one at a time
  for (auto const &[counter_index, counter_type] :
       std::views::enumerate(graph.counters)) {
    bool is_incremented = edge.counters.contains(counter_index);

    size_t const current_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) *
            ((graph.counters.size() + 1) * current_state + counter_index + 1);

    size_t const next_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) * ((graph.counters.size() + 1) * edge.output_index +
                               counter_index + 1);

    // TODO: maybe just issue an add with a memory operand?
    builder.insert_load32(computed_counter_value, current_state_base_reg,
                          current_state_counter_offset);

    if (is_incremented) {
      builder.insert_add(computed_counter_value, 1);
    }

    // test_result = next_state_counter[counter] - computed_counter
    builder.insert_load_cmp32(next_state_base_reg, next_state_counter_offset,
                              /*compare_to=*/computed_counter_value);

    // Fallthrough when equal
    if (counter_type == Counter::greedy) {
      // Abandon when CF=0 & ZF=0
      // Accept when CF=1
      builder.insert_jump_if_carry_flag(
          commit_from_counter_labels[counter_index]);

      if ((size_t)counter_index != graph.counters.size() - 1) {
        builder.insert_jump_if_not_carry_nor_zero_flag(abandon_transition);
      }
    } else {
      // Abandon when CF=1
      // Accept when CF=0 & ZF=0
      builder.insert_jump_if_not_carry_nor_zero_flag(
          commit_from_counter_labels[counter_index]);

      if ((size_t)counter_index != graph.counters.size() - 1) {
        builder.insert_jump_if_carry_flag(abandon_transition);
      }
    }
  }
  // We've failed to accept this transition, we might as well abandon it
  builder.insert_jump(abandon_transition);
  builder.attach_label(commit_new_state);
  compile_commit_new_state(builder, graph, edge, current_state,
                           commit_from_counter_labels);

  builder.attach_label(abandon_transition);
}

auto compile_check_condition(FunctionBuilder &builder,
                             RegexGraphImpl const &graph, Node const *node,
                             Label skip_node_label) -> void {
  auto const &condition = node->condition;
  // Special cases
  if (std::holds_alternative<category::Any>(condition.type)) {
    return; // We always match
  }

  if (std::holds_alternative<Codepoint>(condition.type)) {
    // Long strings are a very common case where the NFA does poorly
    // Cheat by evaluating as many characters as possible simultaneously, this
    // will avoid spurious node activations later in the chain.
    std::string character_chain;
    std::vector<size_t> cumulative_length;
    auto *next_node = node;
    do {
      codepoint_to_utf8(character_chain,
                        std::get<Codepoint>(next_node->condition.type));
      cumulative_length.push_back(character_chain.size());
      if (next_node->edges.size() != 1) {
        break;
      }
      next_node = graph.all_nodes[next_node->edges[0].output_index].get();
    } while (std::holds_alternative<Codepoint>(next_node->condition.type));

    auto const minimum_required_compare_size = cumulative_length[0];

    if (character_chain.size() >= sizeof(uint32_t)) {
      // Multi-character (or single utf-8 string compare)
      uint32_t compare_imm = 0;
      ::memcpy(&compare_imm, character_chain.data(), sizeof(uint32_t));
      builder.insert_cmp_r32_imm32(lookahead_buffer, compare_imm);
      builder.insert_jump_if_not_zero_flag(skip_node_label);
      return;
    }

    if (minimum_required_compare_size == sizeof(uint8_t)) {
      // ASCII compare
      uint8_t compare_imm = 0;
      ::memcpy(&compare_imm, character_chain.data(), sizeof(uint8_t));
      builder.insert_cmp_r8_imm8(lookahead_buffer, compare_imm);
      builder.insert_jump_if_not_zero_flag(skip_node_label);
      return;
    }

    // Slowpath utf-8 compare
    builder.insert_cmp_r32_imm32(codepoint,
                                 std::get<Codepoint>(condition.type).value);
    builder.insert_jump_if_not_zero_flag(skip_node_label);
    return;
  }

  // Fallback to c++ implementation
  builder.insert_push(current_index);
  builder.insert_mov32(CallingConvention::argument[0], codepoint);
  std::visit(
      [&]<typename Condition>(Condition const &condition) {
        builder.insert_load_imm64(CallingConvention::argument[1],
                                  (uint64_t)&condition);
        builder.insert_call(static_cast<bool (*)(Codepoint, Condition const &)>(
            &evaluation::evaluate_condition));
      },
      condition.type);
  // We've just clobbered the current_index... Rematerialize
  builder.insert_pop(current_index);
  builder.insert_jump_if_bool_false(CallingConvention::ret, skip_node_label);
}

auto compile_impl(std::unique_ptr<RegexGraphImpl> graph)
    -> std::unique_ptr<RegexCompiledImpl> {
  Assembler assembler;

  // Build the function to be called every iteration of the loop
  Label entry_point = assembler.allocate_label();
  assembler.build_function(entry_point, [&](auto &builder) {
    // arg[0] = codepoint
    // arg[1] = last_index
    // arg[2] = current_index
    // arg[3] = current_state_base_addr
    // arg[4] = next_state_base_addr
    // arg[5] = lookahead_buffer

    // Respect the c++ calling convention around us
    for (auto reg : CallingConvention::callee_saved) {
      builder.mark_saved(reg);
    }

    builder.insert_mov32(codepoint, CallingConvention::argument[0]);

    builder.insert_mov32(current_index, CallingConvention::argument[2]);
    builder.insert_mov32(indicies, CallingConvention::argument[1]);

    builder.insert_mov64(current_state_base_reg,
                         CallingConvention::argument[3]);
    builder.insert_mov64(next_state_base_reg, CallingConvention::argument[4]);

    builder.insert_mov32(lookahead_buffer, CallingConvention::argument[5]);

    builder.insert_xor(did_accept_any_state, did_accept_any_state);
    for (auto const &[state_index, node] :
         std::views::enumerate(graph->all_nodes)) {
      if (std::holds_alternative<Condition::Entry>(node->condition.type) ||
          std::holds_alternative<Condition::Match>(node->condition.type)) {
        // No need to codegen anything for the placeholder states
        continue;
      }
      auto const skip_node_label = builder.allocate_label();
      // 1) Is our state active?
      // - Last matching index is stored as the first counter
      builder.insert_load_cmp32(
          current_state_base_reg,
          /*offset=*/StateAtIndex::counter_offset(*graph) +
              sizeof(CounterType) * (graph->counters.size() + 1) * state_index,
          /*compare_to=*/indicies);
      builder.insert_jump_if_not_zero_flag(skip_node_label);

      builder.insert_or_imm8(did_accept_any_state, 1);

      // 2) Evaluate our condition
      // - Load the codepoint from TOS
      compile_check_condition(builder, *graph, node.get(), skip_node_label);

      // 3) The condition passes
      for (auto const &edge : node->edges) {
        compile_edge_transition(builder, *graph, edge, state_index);
      }

      builder.attach_label(skip_node_label);
    }

    builder.insert_mov32(CallingConvention::ret, did_accept_any_state);
  });

  assembler.apply_all_fixups();

  auto section = ExecutableSection{assembler.program.data};
  auto entry_point_ptr =
      section.get_fn_ptr<bool, Codepoint, evaluation::IndexType,
                         evaluation::IndexType, void *, void *, unsigned>(
          assembler.label_to_location[entry_point]);
  return std::make_unique<RegexCompiledImpl>(
      std::move(graph), std::move(section), entry_point_ptr);
}
} // namespace

auto uregex::compile(RegexGraph &&graph) -> RegexCompiled {
  return compile_impl(std::move(graph.impl_));
}

auto RegexCompiledImpl::evaluate(uregex::MatchResult &result,
                                 std::string_view text) const -> bool {
  auto all_state = evaluation::EvaluationState{*m_graph};

  auto *current_state = &all_state.m_state_1;
  auto *next_state = &all_state.m_state_2;

  size_t current_index = 0;
  size_t lookahead_buffer = 0;
  if (text.size() < sizeof(lookahead_buffer)) {
    ::memcpy(&lookahead_buffer, text.data(), text.size());
    goto copy_final_subword;
  }

  {
    std::string_view remaining_text = text;
    size_t codepoint_size = 0;
    while (remaining_text.size() >= sizeof(lookahead_buffer)) {
      remaining_text = sub_unchecked(text, current_index);
      Codepoint codepoint = parse_utf8_char(remaining_text, codepoint_size);

      ::memcpy(&lookahead_buffer, remaining_text.data(),
               sizeof(lookahead_buffer));

      auto did_accept_any_state = std::invoke(
          m_entrypoint, codepoint, current_index,
          current_index + codepoint_size, (void *)current_state->counters,
          (void *)next_state->counters, lookahead_buffer);

      if (not did_accept_any_state) {
        return all_state.calculate_match_result(result, next_state, *m_graph,
                                                text);
      }

      current_index += codepoint_size;
      std::swap(current_state, next_state);
    }

    lookahead_buffer >>= 8U * codepoint_size;
  }

copy_final_subword:
  while (current_index < text.size()) {
    size_t codepoint_size = 0;
    Codepoint codepoint =
        parse_utf8_char(sub_unchecked(text, current_index), codepoint_size);

    auto did_accept_any_state = std::invoke(
        m_entrypoint, codepoint, current_index, current_index + codepoint_size,
        (void *)current_state->counters, (void *)next_state->counters,
        lookahead_buffer);

    lookahead_buffer >>= 8U * codepoint_size;

    if (not did_accept_any_state) {
      return all_state.calculate_match_result(result, next_state, *m_graph,
                                              text);
    }

    current_index += codepoint_size;
    std::swap(current_state, next_state);
  }
  return all_state.calculate_match_result(result, current_state, *m_graph,
                                          text);
}
