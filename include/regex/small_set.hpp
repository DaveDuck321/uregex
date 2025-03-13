#pragma once

#include <algorithm>
#include <initializer_list>
#include <limits>
#include <vector>

namespace regex {
template <typename T> class SmallSet {
  // Placeholder implementation
  std::vector<T> m_entries;

public:
  constexpr SmallSet() : m_entries() {
    m_entries.push_back(std::numeric_limits<T>::max());
  }

  constexpr SmallSet(std::initializer_list<T> input) : m_entries(input) {
    m_entries.push_back(std::numeric_limits<T>::max());
  }

  constexpr auto insert(T entry) -> void {
    for (auto const &other_entry : m_entries) {
      if (entry == other_entry) {
        return;
      }
    }
    m_entries.push_back(entry);
    std::sort(m_entries.begin(), m_entries.end());
  }
  constexpr auto operator+(SmallSet const &other) -> SmallSet {
    SmallSet result;
    for (auto const &value : *this) {
      result.insert(value);
    }
    for (auto const &value : other) {
      result.insert(value);
    }
    return result;
  }

  constexpr auto size() const -> size_t { return m_entries.size(); }
  constexpr auto empty() const -> bool { return m_entries.empty(); }

  constexpr auto begin() const { return m_entries.begin(); }
  constexpr auto end() const { return m_entries.end() - 1; }
  constexpr auto back() const -> T const & {
    return m_entries[m_entries.size() - 2];
  };
};
} // namespace regex
