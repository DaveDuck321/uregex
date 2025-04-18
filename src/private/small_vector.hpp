#pragma once

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdlib>
#include <utility>

namespace uregex {
template <typename T, size_t inline_capacity> class SmallVector {
  static constexpr auto initial_allocation_capacity =
      1 << (8 * sizeof(inline_capacity) - std::countl_zero(inline_capacity));
  static_assert(initial_allocation_capacity > inline_capacity);

  size_t m_size;
  union {
    struct {
      T data[inline_capacity];
    } m_inline;
    struct {
      size_t capacity;
      T *data;
    } m_heap;
  };

  constexpr auto is_storage_inline() const -> bool {
    return m_size <= inline_capacity;
  }

public:
  constexpr SmallVector() : m_size{0} {}
  constexpr SmallVector(SmallVector const &) = delete;
  constexpr auto operator=(SmallVector const &) -> SmallVector const & = delete;
  constexpr SmallVector(SmallVector &&other) : m_size{other.m_size} {
    if (other.is_storage_inline()) {
      for (size_t i = 0; i < other.m_size; i += 1) {
        new (&m_inline.data[i]) T{std::move(other.m_inline.data[i])};
      }
    } else {
      m_heap = std::move(other.m_heap);
      other.m_heap.data = nullptr;
    }
  }
  constexpr ~SmallVector() {
    if (is_storage_inline()) {
      for (size_t i = 0; i < m_size; i += 1) {
        m_inline.data[i].~T();
      }
    } else if (m_heap.data != nullptr) {
      for (size_t i = 0; i < m_size; i += 1) {
        m_heap.data[i].~T();
      }
      ::free(m_heap.data);
    }
  }

  template <typename... Args> constexpr auto emplace_back(Args &&...args) {
    if (m_size < inline_capacity) {
      // Room to insert this item inline
      new (&m_inline.data[m_size++]) T{std::forward<Args>(args)...};
    } else if (m_size == inline_capacity) {
      // Spill from inline to out-of-line
      T *new_buffer = (T *)::malloc(sizeof(T) * initial_allocation_capacity);
      for (size_t i = 0; i < m_size; i += 1) {
        new (&new_buffer[i]) T{std::move(m_inline.data[i])};
        m_inline.data[i].~T();
      }
      m_heap = {
          .capacity = initial_allocation_capacity,
          .data = new_buffer,
      };
      new (&m_heap.data[m_size++]) T{std::forward<Args>(args)...};
    } else if (m_size < m_heap.capacity) {
      // Room to insert this item out-of-line
      new (&m_heap.data[m_size++]) T{std::forward<Args>(args)...};
    } else {
      // Spill into a new out-of-line buffer
      size_t new_capacity = 2 * m_heap.capacity;
      T *new_buffer = (T *)::malloc(sizeof(T) * new_capacity);
      for (size_t i = 0; i < m_size; i += 1) {
        new (&new_buffer[i]) T{std::move(m_heap.data[i])};
        m_heap.data[i].~T();
      }
      // TODO: we leak this if we throw in the constructor/ destructor above
      ::free(m_heap.data);

      m_heap = {
          .capacity = new_capacity,
          .data = new_buffer,
      };
      new (&m_heap.data[m_size++]) T{std::forward<Args>(args)...};
    }
  }

  constexpr auto size() const -> size_t { return m_size; }

  constexpr auto begin() const -> T const * {
    if (is_storage_inline()) {
      return m_inline.data;
    } else {
      return m_heap.data;
    }
  }

  constexpr auto end() const -> T const * { return begin() + size(); }

  constexpr auto back() const -> T const & {
    if (is_storage_inline()) {
      return m_inline.data[m_size - 1];
    } else {
      return m_heap.data[m_size - 1];
    }
  }

  constexpr auto operator[](size_t index) -> T & {
    if (is_storage_inline()) {
      return m_inline.data[index];
    } else {
      return m_heap.data[index];
    }
  }

  constexpr auto operator[](size_t index) const -> T const & {
    if (is_storage_inline()) {
      return m_inline.data[index];
    } else {
      return m_heap.data[index];
    }
  }
};
} // namespace uregex
