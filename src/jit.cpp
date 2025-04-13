#include "private/graph_analyser.hpp"
#include "regex/regex.hpp"

#include "private/assembler.hpp"
#include "private/character_categories.hpp"
#include "private/evaluation.hpp"
#include "private/jit.hpp"
#include "private/nfa.hpp"
#include "private/unicode.hpp"

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <ranges>
#include <set>
#include <string.h>
#include <variant>

using namespace uregex;
using namespace uregex::jit;
using namespace uregex::evaluation;
using namespace std::ranges;

namespace {

constexpr auto current_state_base_reg = CallingConvention::callee_saved[0];
constexpr auto next_state_base_reg = CallingConvention::callee_saved[1];

constexpr auto current_index = CallingConvention::callee_saved[2];
constexpr auto codepoint = CallingConvention::callee_saved[3];

constexpr auto did_accept_any_state = CallingConvention::callee_saved[4];
constexpr auto lookahead_buffer = CallingConvention::callee_saved[5];

constexpr auto last_index = CallingConvention::temporary[1];

auto compile_commit_new_state(
    FunctionBuilder &builder, GraphAnalyzer const &analyser,
    RegexGraphImpl const &graph, Edge const &edge, size_t current_state,
    std::vector<Label> const &commit_from_counter_labels) -> void {
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
  bool skip_next = false;
  for (size_t counter_index :
       analyser.get_non_zero_counters(edge.output_index)) {
    if (skip_next) {
      skip_next = false;
      continue;
    }

    size_t const current_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) *
            ((graph.counters.size() + 1) * current_state + counter_index + 1);

    size_t const next_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) * ((graph.counters.size() + 1) * edge.output_index +
                               counter_index + 1);

    bool is_incremented = edge.counters.contains(counter_index);
    bool is_next_incremented = edge.counters.contains(counter_index + 1);
    bool is_next_non_zero =
        not analyser.is_counter_zero(edge.output_index, counter_index + 1);

    bool is_initialized =
        not analyser.is_counter_zero(current_state, counter_index);
    bool is_next_initialized =
        not analyser.is_counter_zero(current_state, counter_index + 1);

    builder.attach_label(commit_from_counter_labels[counter_index]);

    if (not is_initialized) {
      builder.insert_store_imm32(next_state_base_reg, next_state_counter_offset,
                                 is_incremented ? 1 : 0);
      continue;
    }

    if (!is_incremented && !is_next_incremented && is_next_initialized &&
        is_next_non_zero && (current_state_counter_offset % 8) == 0) {
      // Special case: we're just copying over both this counter AND the next
      // counter. We might as well group consecutive 4-byte copies into a single
      // 8-byte copy.
      builder.attach_label(commit_from_counter_labels[counter_index + 1]);

      builder.insert_load64(computed_counter_value, current_state_base_reg,
                            current_state_counter_offset);
      builder.insert_store64(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_counter_offset,
                             /*src=*/computed_counter_value);
      skip_next = true;
      continue;
    }

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

    auto const does_start_group = edge.start_groups.contains(group_index);
    auto const does_end_group = edge.end_groups.contains(group_index);
    auto const is_start_group_unset =
        analyser.is_group_start_unset(edge.output_index, group_index);
    auto const is_end_group_unset =
        analyser.is_group_end_unset(edge.output_index, group_index);

    auto const is_src_group_start_unset =
        analyser.is_group_start_unset(current_state, group_index);
    auto const is_src_group_end_unset =
        analyser.is_group_end_unset(current_state, group_index);

    if (not(does_start_group || does_end_group || is_start_group_unset ||
            is_end_group_unset || is_src_group_start_unset ||
            is_src_group_end_unset)) {
      // Common case where we're not touching this group, copy over the entire
      // struct atomically.
      builder.insert_load64(tmp_group, current_state_base_reg,
                            current_state_group_offset);
      builder.insert_store64(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset,
                             /*src=*/tmp_group);
      continue;
    }

    if (does_start_group) {
      builder.insert_store32(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset +
                                 offsetof(Group, start_index),
                             /*src=*/current_index);
    } else {
      if (not is_start_group_unset && is_src_group_start_unset) {
        builder.insert_store64_simm32(/*dst_base=*/next_state_base_reg,
                                      /*dst_offset=*/next_state_group_offset +
                                          offsetof(Group, start_index),
                                      -1);
      } else if (not is_start_group_unset) {
        builder.insert_load32(tmp_group, current_state_base_reg,
                              current_state_group_offset +
                                  offsetof(Group, start_index));
        builder.insert_store32(/*dst_base=*/next_state_base_reg,
                               /*dst_offset=*/next_state_group_offset +
                                   offsetof(Group, start_index),
                               /*src=*/tmp_group);
      }
    }

    if (does_end_group) {
      builder.insert_store32(/*dst_base=*/next_state_base_reg,
                             /*dst_offset=*/next_state_group_offset +
                                 offsetof(Group, end_index),
                             /*src=*/current_index);
    } else {
      if (not is_end_group_unset && is_src_group_end_unset) {
        builder.insert_store64_simm32(/*dst_base=*/next_state_base_reg,
                                      /*dst_offset=*/next_state_group_offset +
                                          offsetof(Group, end_index),
                                      -1);
      } else if (not is_end_group_unset) {
        builder.insert_load32(tmp_group, current_state_base_reg,
                              current_state_group_offset +
                                  offsetof(Group, end_index));
        builder.insert_store32(/*dst_base=*/next_state_base_reg,
                               /*dst_offset=*/next_state_group_offset +
                                   offsetof(Group, end_index),
                               /*src=*/tmp_group);
      }
    }
  }
}

auto compile_edge_transition(FunctionBuilder &builder,
                             GraphAnalyzer const &analyser,
                             RegexGraphImpl const &graph, Edge const &edge,
                             std::set<size_t> &has_first_transition_to_state,
                             size_t current_state, Label abandon_transition)
    -> void {
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

  // Special case: we know the `current_index` check will always pass if we're
  // the first transition into this node. In this case, don't bother with the
  // codegen of any counter compares.
  if (has_first_transition_to_state.count(edge.output_index) == 0) {
    has_first_transition_to_state.insert(edge.output_index);
    compile_commit_new_state(builder, analyser, graph, edge, current_state,
                             commit_from_counter_labels);
    return;
  }

  auto const commit_new_state = builder.allocate_label();

  // - The first counter is set to the current index if we've already
  // added the state at this iteration.
  builder.insert_load_cmp32(
      next_state_base_reg,
      /*offset=*/StateAtIndex::counter_offset(graph) +
          sizeof(CounterType) * (graph.counters.size() + 1) * edge.output_index,
      /*compare_to=*/current_index);
  builder.insert_jump_if_not_zero_flag(commit_new_state);

  auto const &non_zero_counters =
      analyser.get_non_zero_counters(edge.output_index);

  // Now we need to consider the remaining counters one at a time
  for (size_t counter_index : non_zero_counters) {
    auto counter_type = graph.counters[counter_index];
    size_t const current_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) *
            ((graph.counters.size() + 1) * current_state + counter_index + 1);

    size_t const next_state_counter_offset =
        StateAtIndex::counter_offset(graph) +
        sizeof(CounterType) * ((graph.counters.size() + 1) * edge.output_index +
                               counter_index + 1);

    builder.insert_load32(computed_counter_value, current_state_base_reg,
                          current_state_counter_offset);

    bool is_incremented = edge.counters.contains(counter_index);
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
  compile_commit_new_state(builder, analyser, graph, edge, current_state,
                           commit_from_counter_labels);
}

struct CheckCondition {
  FunctionBuilder *builder;
  RegexGraphImpl const *graph;
  Node const *node;

  auto operator()(category::Any const &, Label target, bool jump_on_pass)
      -> void {
    if (jump_on_pass) {
      // We always pass (and therefore never fallthrough)
      builder->insert_jump(target);
    }
  }

  auto operator()(Codepoint const &condition, Label target, bool jump_on_pass)
      -> void {
    // Long strings are a very common case where the NFA does poorly
    // Cheat by evaluating as many characters as possible simultaneously, this
    // will avoid spurious node activations later in the chain.
    std::string character_chain;
    codepoint_to_utf8(character_chain, condition);

    auto *next_node = node;
    while (next_node->edges.size() == 1) {
      next_node = graph->all_nodes[next_node->edges[0].output_index].get();
      if (not std::holds_alternative<Codepoint>(next_node->condition.type)) {
        break;
      }

      codepoint_to_utf8(character_chain,
                        std::get<Codepoint>(next_node->condition.type));
    }

    if (character_chain.size() >= sizeof(uint64_t) && !jump_on_pass) {
      // Multi-character
      uint64_t compare_imm = 0;
      ::memcpy(&compare_imm, character_chain.data(), sizeof(uint64_t));
      builder->insert_load_imm64(CallingConvention::temporary[0], compare_imm);
      builder->insert_cmp64(lookahead_buffer, CallingConvention::temporary[0]);
    } else if (character_chain.size() >= sizeof(uint32_t) && !jump_on_pass) {
      // Multi-character (or single utf-8 string compare)
      uint32_t compare_imm = 0;
      ::memcpy(&compare_imm, character_chain.data(), sizeof(uint32_t));
      builder->insert_cmp_r32_imm32(lookahead_buffer, compare_imm);
    } else {
      // Slowpath utf-8 compare
      builder->insert_cmp_r32_imm32(codepoint, condition.value);
    }

    if (jump_on_pass) {
      builder->insert_jump_if_zero_flag(target);
    } else {
      builder->insert_jump_if_not_zero_flag(target);
    }
  }

  auto operator()(Condition::CustomExpression const &custom_expression,
                  Label target_if_failed, bool jump_on_pass) -> void {
    assert(not jump_on_pass);
    if (custom_expression.is_complement) {
      // [^ab] fails if either a or b passes
      for (auto const &part : custom_expression.expressions) {
        std::visit(
            [&](auto const &condition) {
              return (*this)(condition, target_if_failed,
                             /*jump_on_pass=*/true);
            },
            part);
      }
      return;
    }

    // [ab] passes if either a or b passes
    auto pass_target = builder->allocate_label();
    for (auto const &[index, part] :
         std::views::enumerate(custom_expression.expressions)) {
      std::visit(
          [&](auto const &condition) {
            if ((size_t)index + 1 == custom_expression.expressions.size()) {
              // We're the last expression, we should fallthrough like a normal
              // condition
              return (*this)(condition, target_if_failed,
                             /*jump_on_pass=*/false);
            } else {
              return (*this)(condition, pass_target,
                             /*jump_on_pass=*/true);
            }
          },
          part);
    }
    builder->attach_label(pass_target);
  }

  template <typename Condition>
  auto operator()(Condition const &condition, Label target, bool jump_on_pass)
      -> void {
    // Fallback to c++ implementation
    builder->insert_push(last_index);
    builder->insert_mov32(CallingConvention::argument[0], codepoint);

    if (not meta::is_empty<Condition>) {
      builder->insert_load_imm64(CallingConvention::argument[1],
                                 (uint64_t)&condition);
    }
    builder->insert_call(static_cast<bool (*)(Codepoint, Condition const &)>(
        &evaluation::evaluate_condition));

    // c++ might have clobbered current_index
    builder->insert_pop(last_index);
    if (jump_on_pass) {
      builder->insert_jump_if_bool_true(CallingConvention::ret, target);
    } else {
      builder->insert_jump_if_bool_false(CallingConvention::ret, target);
    }
  }
};

auto compile_impl(std::unique_ptr<RegexGraphImpl> graph)
    -> std::unique_ptr<RegexCompiledImpl> {
  Assembler assembler;

  GraphAnalyzer analyser{*graph};

  std::set<size_t> has_first_transition_to_state;

  // The first state is special, lets compile that separately
  Label init_entry_point = assembler.allocate_label();
  assembler.build_function(init_entry_point, [&](auto &builder) {
    // arg[0] = current_index
    // arg[1] = current_state_base_addr
    // arg[2] = next_state_base_addr

    for (auto reg : CallingConvention::callee_saved) {
      builder.mark_saved(reg);
    }

    builder.insert_mov32(current_index, CallingConvention::argument[0]);
    builder.insert_mov64(current_state_base_reg,
                         CallingConvention::argument[1]);
    builder.insert_mov64(next_state_base_reg, CallingConvention::argument[2]);

    for (auto const &edge : graph->entry->edges) {
      auto abandon_transition_label = builder.allocate_label();
      compile_edge_transition(builder, analyser, *graph, edge,
                              has_first_transition_to_state,
                              graph->entry->index, abandon_transition_label);
      builder.attach_label(abandon_transition_label);
    }
  });

  std::map<size_t, Label> state_evaluation_body_labels;
  has_first_transition_to_state.clear();

  // Every other state can be compiled separately
  std::vector<Label> state_start_labels;
  for (size_t state_id = 0; state_id < graph->all_nodes.size(); state_id += 1) {
    state_start_labels.push_back(assembler.allocate_label());
  }
  // Allocate a final index for the last state
  state_start_labels.push_back(assembler.allocate_label());

  // Build the matching function body
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

    builder.insert_mov32(last_index, CallingConvention::argument[1]);
    builder.insert_mov32(current_index, CallingConvention::argument[2]);

    builder.insert_mov64(current_state_base_reg,
                         CallingConvention::argument[3]);
    builder.insert_mov64(next_state_base_reg, CallingConvention::argument[4]);

    builder.insert_mov64(lookahead_buffer, CallingConvention::argument[5]);

    builder.insert_xor(did_accept_any_state, did_accept_any_state);

    for (auto const &[state_index, node] :
         std::views::enumerate(graph->all_nodes)) {
      builder.attach_label(state_start_labels[state_index]);
      if (std::holds_alternative<Condition::Entry>(node->condition.type) ||
          std::holds_alternative<Condition::Match>(node->condition.type)) {
        // No need to codegen anything for the placeholder states
        continue;
      }

      // 1) Is our state active?
      // - Last matching index is stored as the first counter
      builder.insert_load_cmp32(
          current_state_base_reg,
          /*offset=*/StateAtIndex::counter_offset(*graph) +
              sizeof(CounterType) * (graph->counters.size() + 1) * state_index,
          /*compare_to=*/last_index);

      state_evaluation_body_labels[state_index] = builder.allocate_label();

      // 2) Jump to the evaluation body
      builder.insert_jump_if_zero_flag(
          state_evaluation_body_labels[state_index]);
    }

    builder.attach_label(state_start_labels[graph->all_nodes.size()]);
    builder.insert_mov32(CallingConvention::ret, did_accept_any_state);
  });

  // Insert the bodies for the out-of-line state evaluation
  for (auto const &[state_index, label] : state_evaluation_body_labels) {
    assembler.build_out_of_line_block(label, [&](auto &builder) {
      auto const *node = graph->all_nodes[state_index].get();

      // 1) Evaluate our condition
      CheckCondition condition_overloads{&builder, graph.get(), node};
      std::visit(
          [&](auto const &condition) {
            condition_overloads(condition, state_start_labels[state_index + 1],
                                /*jump_on_pass=*/false);
          },
          node->condition.type);

      // 2) If we're here, the state is active AND the condition passes.
      builder.insert_or_imm8(did_accept_any_state, 1);

      size_t next_state_index = state_index + 1;
      while (
          next_state_index != graph->all_nodes.size() &&
          are_mutually_exclusive(
              node->condition, graph->all_nodes[next_state_index]->condition)) {
        next_state_index += 1;
      }

      for (auto const &edge : node->edges) {
        bool is_final_edge = (&node->edges.back() == &edge);
        Label abandon_transition = is_final_edge
                                       ? state_start_labels[next_state_index]
                                       : builder.allocate_label();

        compile_edge_transition(builder, analyser, *graph, edge,
                                has_first_transition_to_state, state_index,
                                abandon_transition);

        if (not is_final_edge) {
          builder.attach_label(abandon_transition);
        }
      }

      // 3) Jump back to the main dispatch block
      builder.insert_jump(state_start_labels[next_state_index]);
    });
  }

  assembler.apply_all_fixups();

  // Clear any state remaining from the c++ engine's last run
  auto _ = evaluation::EvaluationState{*graph, true};
  auto section = ExecutableSection{assembler.m_program.data};
  auto init_ptr =
      section.get_fn_ptr<void, evaluation::IndexType, void *, void *>(
          assembler.m_labels[+init_entry_point].location);
  auto eval_ptr =
      section.get_fn_ptr<bool, Codepoint, evaluation::IndexType,
                         evaluation::IndexType, void *, void *, size_t>(
          assembler.m_labels[+entry_point].location);
  return std::make_unique<RegexCompiledImpl>(
      std::move(graph), std::move(section), init_ptr, eval_ptr);
}
} // namespace

auto uregex::compile(RegexGraph &&graph) -> RegexCompiled {
  return compile_impl(std::move(graph.impl_));
}

auto RegexCompiledImpl::evaluate(uregex::MatchResult &result,
                                 std::string_view text) const -> bool {
  result.text = text;
  result.groups.clear();
  result.did_match = false;

  auto all_state = evaluation::EvaluationState{*m_graph, false};

  //  The next (definitively unused) offset is previous + max unicode size + 1
  size_t const current_uniform_offset = m_current_index + 5;
  size_t const end_index = current_uniform_offset + text.size();
  char const *text_cursor = text.data();
  size_t current_index = current_uniform_offset;

  auto *current_state = &all_state.m_state_1;
  auto *next_state = &all_state.m_state_2;

  std::invoke(m_init_entrypoint, current_index, (void *)current_state->counters,
              (void *)next_state->counters);

  std::swap(current_state, next_state);

  size_t lookahead_buffer = 0;
  if (current_index + sizeof(lookahead_buffer) > end_index) {
    ::memcpy(&lookahead_buffer, text.data(), text.size());
    goto copy_final_subword;
  }

  {
    size_t codepoint_size = 0;
    while (current_index + sizeof(lookahead_buffer) <= end_index) {
      Codepoint codepoint = parse_utf8_char(text_cursor, codepoint_size);

      ::memcpy(&lookahead_buffer, text_cursor, sizeof(lookahead_buffer));

      auto did_accept_any_state = m_eval_entrypoint(
          codepoint, current_index, current_index + codepoint_size,
          (void *)current_state->counters, (void *)next_state->counters,
          lookahead_buffer);

      if (not did_accept_any_state) [[unlikely]] {
        m_current_index = current_index;
        return false;
      }

      current_index += codepoint_size;
      text_cursor += codepoint_size;
      std::swap(current_state, next_state);
    }

    lookahead_buffer >>= 8U * codepoint_size;
  }

copy_final_subword:
  while (current_index < end_index) {
    size_t codepoint_size = 0;
    Codepoint codepoint = parse_utf8_char(text_cursor, codepoint_size);

    auto did_accept_any_state = m_eval_entrypoint(
        codepoint, current_index, current_index + codepoint_size,
        (void *)current_state->counters, (void *)next_state->counters,
        lookahead_buffer);

    lookahead_buffer >>= 8U * codepoint_size;

    if (not did_accept_any_state) [[unlikely]] {
      m_current_index = current_index;
      return false;
    }

    current_index += codepoint_size;
    text_cursor += codepoint_size;
    std::swap(current_state, next_state);
  }

  m_current_index = current_index;
  return all_state.calculate_match_result(result, current_state, *m_graph, text,
                                          current_uniform_offset);
}
