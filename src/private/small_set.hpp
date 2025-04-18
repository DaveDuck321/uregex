#pragma once

#include <algorithm>
#include <cstdint>
#include <limits>
#include <type_traits>

namespace uregex {
template <typename T> class SmallSet {
  static_assert(sizeof(T) == sizeof(uint32_t));
  static_assert(std::is_trivial_v<T>);

  static constexpr uint32_t max_inline_elements =
      (sizeof(T *) + sizeof(uint32_t)) / sizeof(uint32_t);
  static constexpr uint32_t initial_capacity = 8;
  static_assert(initial_capacity > max_inline_elements);

  union {
    struct {
      uint32_t size;
      uint32_t capacity;
      T *data;
    } m_heap;
    struct {
      uint32_t size;
      T data[max_inline_elements];
    } m_inline;
  };

  constexpr auto is_storage_inline() const -> bool {
    return m_inline.size <= max_inline_elements;
  }

  constexpr auto small_contains(T entry) const -> bool {
    for (uint32_t i = 0; i < m_inline.size; i += 1) {
      if (m_inline.data[i] == entry) {
        return true;
      }
    }
    return false;
  }

  constexpr auto big_contains(T entry) const -> bool {
    // TODO: maybe binary search here
    for (uint32_t i = 0; i < m_heap.size; i += 1) {
      if (m_heap.data[i] == entry) {
        return true;
      }
    }
    return false;
  }

  template <bool do_deduplicate = true>
  static constexpr auto maybe_sorted_insert(T *data, uint32_t &size, T entry)
      -> void {
    T placing = entry;
    for (size_t i = 0; i < size; i += 1) {
      if constexpr (do_deduplicate) {
        if (placing == data[i]) {
          return;
        }
      }

      if (placing < data[i]) {
        std::swap(placing, data[i]);
      }
    }
    data[size++] = placing;
  }

  static constexpr auto unchecked_sorted_insert(T *data, uint32_t &size,
                                                T entry) -> void {
    maybe_sorted_insert</*do_deduplicate=*/false>(data, size, entry);
  }

  constexpr auto insert_small(T entry) -> void {
    maybe_sorted_insert(m_inline.data, m_inline.size, entry);
  }

  constexpr auto insert_small_and_spill(T entry) -> void {
    if (small_contains(entry)) {
      return;
    }

    T *buffer = new T[initial_capacity];
    std::copy(m_inline.data, m_inline.data + max_inline_elements, buffer);

    uint32_t size = max_inline_elements;
    unchecked_sorted_insert(buffer, size, entry);

    m_heap = {
        .size = size,
        .capacity = initial_capacity,
        .data = buffer,
    };
  }

  constexpr auto insert_big(T entry) -> void {
    // TODO: maybe use a more efficient heap data-structure after reallocation
    maybe_sorted_insert(m_heap.data, m_heap.size, entry);
  }

  constexpr auto insert_big_and_realloc(T entry) -> void {
    if (big_contains(entry)) {
      return;
    }

    m_heap.capacity *= 2;

    T *old_data = m_heap.data;
    m_heap.data = new T[m_heap.capacity];
    std::copy(old_data, old_data + m_heap.size, m_heap.data);
    delete[] old_data;

    unchecked_sorted_insert(m_heap.data, m_heap.size, entry);
  }

public:
  constexpr SmallSet()
      : m_inline{.size = 1, .data = {std::numeric_limits<T>::max(), 0, 0}} {}

  constexpr SmallSet(SmallSet &&other) : m_heap{other.m_heap} {
    if (not is_storage_inline()) {
      other.m_heap.data = nullptr;
    }
  }

  constexpr SmallSet(SmallSet const &other) : m_heap{other.m_heap} {
    if (not is_storage_inline()) {
      m_heap.data = new T[m_heap.capacity];
      std::copy(other.m_heap.data, other.m_heap.data + other.m_heap.size,
                m_heap.data);
    }
  }

  constexpr auto operator=(SmallSet const &other) -> SmallSet & {
    if (not is_storage_inline() && m_heap.data != nullptr) {
      delete[] m_heap.data;
    }

    m_heap = other.m_heap;

    if (not is_storage_inline()) {
      m_heap.data = new T[m_heap.capacity];
      std::copy(other.m_heap.data, other.m_heap.data + other.m_heap.size,
                m_heap.data);
    }
    return *this;
  };

  constexpr ~SmallSet() {
    if (not is_storage_inline() && m_heap.data != nullptr) {
      delete[] m_heap.data;
    }
  }

  constexpr auto insert(T entry) -> void {
    if (m_inline.size < max_inline_elements) {
      // We fit in the inline storage!
      insert_small(entry);
    } else if (m_inline.size == max_inline_elements) {
      insert_small_and_spill(entry);
    } else if (m_heap.size < m_heap.capacity) {
      // We fit into the extended storage

      insert_big(entry);
    } else {
      // We've just spilled out of the extended storage
      insert_big_and_realloc(entry);
    }
  }
  constexpr auto operator+(SmallSet const &other) -> SmallSet {
    SmallSet result = *this;
    for (auto const &value : other) {
      result.insert(value);
    }
    return result;
  }

  constexpr auto contains(T item) const -> bool {
    if (is_storage_inline()) {
      return small_contains(item);
    } else {
      return big_contains(item);
    }
  }
  constexpr auto size() const -> size_t { return m_inline.size - 1; }
  constexpr auto empty() const -> bool { return m_inline.size == 1; }

  constexpr auto begin() const -> T const * {
    if (is_storage_inline()) {
      return m_inline.data;
    } else {
      return m_heap.data;
    }
  }

  constexpr auto end() const { return begin() + size(); }
  constexpr auto back() const -> T const & {
    if (is_storage_inline()) {
      return m_inline.data[m_inline.size - 2];
    } else {
      return m_heap.data[m_heap.size - 2];
    }
  };
};
static_assert(sizeof(SmallSet<uint32_t>) == 2 * sizeof(void *));
} // namespace uregex
