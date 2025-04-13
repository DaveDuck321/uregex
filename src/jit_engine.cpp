#include "private/jit.hpp"

using namespace uregex;

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

  m_init_entrypoint(current_index, (void *)current_state->counters,
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
