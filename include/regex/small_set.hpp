#pragma once

#include <vector>

namespace regex {
template <typename T> class SmallSet {
  // Placeholder implementation
  std::vector<T> m_entries;

public:
  auto insert(T entry) -> void {
    for (auto const &other_entry : m_entries) {
      if (entry == other_entry) {
        return;
      }
    }
    m_entries.push_back(entry);
  }

  auto begin() const { return m_entries.begin(); }
  auto end() const { return m_entries.end(); }
};
} // namespace regex
