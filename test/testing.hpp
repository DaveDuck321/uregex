#pragma once

#include <string>
#include <unistd.h>

namespace testing {
auto check_impl(bool assertion, std::string_view file, size_t line) -> void;
auto require_impl(bool assertion, std::string_view file, size_t line) -> void;

struct TestRegistrar {
  TestRegistrar(std::string name, std::string tags, void (*test_fn)(void),
                std::string_view file, size_t line);
};
} // namespace testing

#define CHECK(assertion)                                                       \
  do {                                                                         \
    testing::check_impl((assertion), __FILE__, __LINE__);                      \
  } while (false)

#define REQUIRE(assertion)                                                     \
  do {                                                                         \
    testing::require_impl((assertion), __FILE__, __LINE__);                    \
  } while (false)

#define TEST_CASE(name, tags)                                                  \
  auto test_fn_##name()->void;                                                 \
  static testing::TestRegistrar test_fn_register_##name{                       \
      #name, (tags), &test_fn_##name, __FILE__, __LINE__};                     \
  auto test_fn_##name()->void
