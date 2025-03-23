#pragma once

#include "private/evaluation.hpp"
#include "private/nfa.hpp"
#include "private/unicode.hpp"

#include <memory>
#include <sys/mman.h>

namespace uregex {
namespace jit {
class ExecutableSection {
  void *m_executable;
  size_t m_size;

public:
  explicit ExecutableSection(std::span<uint8_t const> data)
      : m_executable{nullptr}, m_size{data.size()} {
    // Map the binary as exec-only
    int fd = ::memfd_create("exec_section", 0);
    ::write(fd, data.data(), data.size());
    m_executable = ::mmap(nullptr, data.size(), PROT_EXEC, MAP_PRIVATE, fd, 0);
    m_size = data.size();
    ::close(fd);
  }
  ExecutableSection(ExecutableSection const &) = delete;
  ExecutableSection(ExecutableSection &&other)
      : m_executable{other.m_executable}, m_size{other.m_size} {
    other.m_executable = nullptr;
    other.m_size = 0;
  }

  auto operator=(ExecutableSection const &) -> ExecutableSection & = delete;
  ~ExecutableSection() {
    if (m_executable != nullptr) {
      ::munmap(m_executable, m_size);
    }
  }

  template <typename Ret, typename... Args>
  auto get_fn_ptr(size_t offset) -> Ret (*)(Args...) {
    intptr_t address = (intptr_t)m_executable + offset;
    return reinterpret_cast<Ret (*)(Args...)>(address);
  }
};
} // namespace jit

struct RegexCompiledImpl {
  using EntryPointFn = bool (*)(Codepoint, evaluation::IndexType,
                                evaluation::IndexType, void *, void *);
  std::unique_ptr<RegexGraphImpl> m_graph;
  jit::ExecutableSection m_executable;
  EntryPointFn m_entrypoint;

  RegexCompiledImpl(std::unique_ptr<RegexGraphImpl> graph,
                    jit::ExecutableSection &&section, EntryPointFn fn)
      : m_graph{std::move(graph)}, m_executable{std::move(section)},
        m_entrypoint{fn} {};

  auto evaluate(std::string_view text) const -> MatchResult;
};
} // namespace uregex
