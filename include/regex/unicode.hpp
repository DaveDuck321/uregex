#pragma once

#include "common.hpp"

#include <cstdint>
#include <string_view>
#include <unistd.h>

namespace regex {
struct Codepoint {
  unsigned value;

  constexpr auto operator==(Codepoint const &) const -> bool = default;

  static constexpr auto invalid_sentinel() -> Codepoint { return {0xfffdU}; };
};

constexpr auto parse_utf8_char(std::string_view text, size_t &offset)
    -> Codepoint {
  offset = 0;
  auto eat_next = [&]() -> unsigned { return (uint8_t)text[offset++]; };

  // NOTE: quick and dirty... This is not correct
  unsigned first_byte = eat_next();
  if ((first_byte & 0x80U) == 0U) {
    // ASCII range
    return {first_byte};
  }

  unsigned second_byte = eat_next();
  if ((first_byte & 0xe0U) == 0xc0U) {
    // 2-bytes
    return {(second_byte & 0x3fU) + ((first_byte & 0x1fU) << 6U)};
  }

  unsigned third_byte = eat_next();
  if ((first_byte & 0xf0U) == 0xe0U) {
    // 3-bytes
    return {(third_byte & 0x3fU) + ((second_byte & 0x3fU) << 6U) +
            ((first_byte & 0x0fU) << 12U)};
  }

  unsigned fourth_byte = eat_next();
  if ((first_byte & 0xf8U) == 0xf0U) {
    // 4-bytes
    return {(fourth_byte & 0x3fU) + ((third_byte & 0x3fU) << 6U) +
            ((second_byte & 0x3fU) << 12U) + ((first_byte & 0x07U) << 18U)};
  }
  return Codepoint::invalid_sentinel();
}

constexpr auto codepoint_to_utf8(std::string &output, Codepoint codepoint)
    -> void {
  if (codepoint.value <= 0x7f) {
    output.push_back(codepoint.value);
    return;
  }

  if (codepoint.value <= 0x07ff) {
    output.push_back(0xc0 | ((codepoint.value >> 6) & 0x1f));
    output.push_back(0x80 | (codepoint.value & 0x3f));
    return;
  }

  if (codepoint.value <= 0xd7ff ||
      (0xe000 <= codepoint.value && codepoint.value <= 0xffff)) {
    output.push_back(0xe0 | ((codepoint.value >> 12) & 0x0f));
    output.push_back(0x80 | ((codepoint.value >> 6) & 0x3f));
    output.push_back(0x80 | (codepoint.value & 0x3f));
  }

  if (0x10000 <= codepoint.value && codepoint.value <= 0x10ffff) {
    output.push_back(0xf0 | ((codepoint.value >> 18) & 0x07));
    output.push_back(0x80 | ((codepoint.value >> 12) & 0x3f));
    output.push_back(0x80 | ((codepoint.value >> 6) & 0x3f));
    output.push_back(0x80 | (codepoint.value & 0x3f));
  }

  throw RegexError("Cannot incode invalid codepoint {:x}", codepoint.value);
}

auto get_category(Codepoint codepoint) -> std::array<char, 2>;
} // namespace regex
