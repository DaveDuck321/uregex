#pragma once

#include "private/evaluation.hpp"
#include "private/nfa.hpp"
#include "private/unicode.hpp"

#include <cstdint>
#include <memory>
#include <mutex>
#include <sys/mman.h>

namespace uregex {
namespace jit {
class JITDumper {
  std::mutex m_jitdump_body;
  size_t m_regex_index;
  void *m_mapped_region;
  int m_fd;

public:
  JITDumper();
  JITDumper(JITDumper const &) = delete;
  auto operator=(JITDumper const &) -> JITDumper & = delete;
  ~JITDumper();
  auto record_load(std::span<uint8_t const> data, void *loaded_to) -> void;
};

class ExecutableSection {
  void *m_executable;
  size_t m_size;

public:
  explicit ExecutableSection(std::span<uint8_t const> data)
      : m_executable{nullptr}, m_size{data.size()} {
    // Map the binary as exec-only
    int fd = ::memfd_create("exec_section", 0);
    ::write(fd, data.data(), data.size());
    m_executable =
        ::mmap(nullptr, data.size(), PROT_EXEC | PROT_READ, MAP_PRIVATE, fd, 0);
    m_size = data.size();
    ::close(fd);

#ifdef JITDUMP_PROFILE
    static JITDumper dumper{};
    dumper.record_load(data, m_executable);
#endif
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

  auto evaluate(MatchResult &result, std::string_view text) const -> bool;
};
} // namespace uregex
