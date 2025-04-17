#pragma once

#include <cstddef>
#include <cstdint>
#include <sys/mman.h>
#include <unistd.h>
#include <utility>

namespace uregex {

inline auto round_up_to_page_size(size_t number) {
  int64_t mask = ::getpagesize() - 1;
  return (number + mask) & ~mask;
}

class AlignedData {
  size_t m_data_size;
  size_t m_minimum_size;
  void *m_data;

public:
  explicit AlignedData(std::nullptr_t) : m_data{nullptr} {}

  explicit AlignedData(size_t minimum_size)
      : m_data_size{round_up_to_page_size(minimum_size)},
        m_minimum_size{minimum_size},
        m_data{::mmap(nullptr, m_data_size, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)} {};

  AlignedData(AlignedData const &) = delete;
  AlignedData(AlignedData &&other)
      : m_data_size{other.m_data_size}, m_minimum_size{other.m_minimum_size},
        m_data{other.m_data} {
    other.m_data = nullptr;
  }

  auto operator=(AlignedData const &) -> AlignedData & = delete;
  auto operator=(AlignedData &&other) -> AlignedData & {
    std::swap(m_data_size, other.m_data_size);
    std::swap(m_minimum_size, other.m_minimum_size);
    std::swap(m_data, other.m_data);
    return *this;
  }

  ~AlignedData() {
    if (m_data) {
      ::munmap(m_data, m_data_size);
    }
  }

  auto data() const -> uint8_t * { return (uint8_t *)m_data; }
  auto minimum_size() const -> size_t { return m_minimum_size; }
};
} // namespace uregex
