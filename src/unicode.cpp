#include "private/unicode.hpp"

#include <array>

[[gnu::section("unicode_category_data"), gnu::aligned(4096)]]
const std::array<std::array<char, 2>, 0x110000> unicode_categories = {};

auto regex::get_category(Codepoint codepoint) -> std::array<char, 2> {
  if (codepoint.value >= unicode_categories.size()) {
    return {'C', 'u'};
  }
  return unicode_categories[codepoint.value];
}
