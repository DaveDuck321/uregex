#pragma once

#include <initializer_list>
#include <vector>

namespace regex {
template <typename T> class SmallSet {
  // Placeholder implementation
  std::vector<T> m_entries;

public:
  constexpr SmallSet() = default;
  constexpr SmallSet(std::initializer_list<T> input) : m_entries(input) {}

  constexpr auto insert(T entry) -> void {
    for (auto const &other_entry : m_entries) {
      if (entry == other_entry) {
        return;
      }
    }
    m_entries.push_back(entry);
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

  constexpr auto empty() const -> bool { return m_entries.empty(); }

  constexpr auto begin() const { return m_entries.begin(); }
  constexpr auto end() const { return m_entries.end(); }
  constexpr auto back() const -> T const & { return m_entries.back(); };
};
} // namespace regex
