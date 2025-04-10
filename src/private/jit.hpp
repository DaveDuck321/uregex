#pragma once

#include "private/evaluation.hpp"
#include "private/nfa.hpp"
#include "private/unicode.hpp"

#include <cstdint>
#include <memory>

namespace uregex {
namespace jit {
class ExecutableSection {
  void *m_executable;
  size_t m_size;

public:
  explicit ExecutableSection(std::span<uint8_t const> data);
  ExecutableSection(ExecutableSection const &) = delete;
  ExecutableSection(ExecutableSection &&other);

  auto operator=(ExecutableSection const &) -> ExecutableSection & = delete;
  ~ExecutableSection();

  template <typename Ret, typename... Args>
  auto get_fn_ptr(size_t offset) -> Ret (*)(Args...) {
    intptr_t address = (intptr_t)m_executable + offset;
    return reinterpret_cast<Ret (*)(Args...)>(address);
  }
};
} // namespace jit

struct RegexCompiledImpl {
  using InitFn = void (*)(evaluation::IndexType, void *, void *);
  using EvalFn = bool (*)(Codepoint, evaluation::IndexType,
                          evaluation::IndexType, void *, void *, size_t);
  std::unique_ptr<RegexGraphImpl> m_graph;
  jit::ExecutableSection m_executable;
  InitFn m_init_entrypoint;
  EvalFn m_eval_entrypoint;

  mutable size_t m_current_index = 0;

  RegexCompiledImpl(std::unique_ptr<RegexGraphImpl> graph,
                    jit::ExecutableSection &&section, InitFn init_fn,
                    EvalFn eval_fn)
      : m_graph{std::move(graph)}, m_executable{std::move(section)},
        m_init_entrypoint{init_fn}, m_eval_entrypoint{eval_fn} {};

  auto evaluate(MatchResult &result, std::string_view text) const -> bool;
};
} // namespace uregex
