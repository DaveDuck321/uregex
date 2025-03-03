#include "testing.hpp"

#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <string_view>
#include <vector>

struct RequireFailedException {};

struct Test {
  std::string name;
  std::string tags;
  std::string_view file;
  size_t line;

  void (*fn)(void);
};

struct FailedCheck {
  std::string_view type;
  std::string_view file;
  size_t line;
};

static size_t checks_passed = 0;
static size_t checks_failed = 0;
static std::vector<Test> *all_tests = nullptr;
static std::vector<FailedCheck> failed_checks;

auto testing::check_impl(bool assertion, std::string_view file, size_t line)
    -> void {
  if (assertion) {
    checks_passed += 1;
  } else {
    checks_failed += 1;
    failed_checks.push_back({.type = "check", .file = file, .line = line});
  }
}

auto testing::require_impl(bool assertion, std::string_view file, size_t line)
    -> void {
  if (assertion) {
    checks_passed += 1;
  } else {
    checks_failed += 1;
    failed_checks.push_back({.type = "require", .file = file, .line = line});
    throw RequireFailedException{};
  }
}

testing::TestRegistrar::TestRegistrar(std::string name, std::string tags,
                                      void (*test_fn)(void),
                                      std::string_view file, size_t line) {
  // Avoid any static initialization order problems
  static std::vector<Test> registered_tests;
  all_tests = &registered_tests;

  registered_tests.push_back(Test{
      .name = name,
      .tags = tags,
      .file = file,
      .line = line,
      .fn = test_fn,
  });
}

int main(int argc, char *argv[]) {
  if (argc != 1 && argc != 2) {
    std::cerr << "Usage: test.out [filter]" << std::endl;
    return EXIT_FAILURE;
  }

  if (all_tests == nullptr) {
    std::print(std::cerr, "Failed: no tests registered\n");
    return EXIT_FAILURE;
  }

  std::string run_if_contains;
  if (argc == 2) {
    run_if_contains = argv[1];
  }

  size_t tests_run = 0;
  std::vector<Test const *> failed_tests;
  for (auto const &test : *all_tests) {
    // Don't use regex here for obvious reasons :-)
    if (not test.tags.contains(run_if_contains)) {
      continue;
    }

    size_t checks_passed_before_test = checks_passed;
    size_t checks_failed_before_test = checks_failed;

    try {
      test.fn();
    } catch (RequireFailedException const &) {
    }
    tests_run += 1;

    size_t assertions_passed = checks_passed - checks_passed_before_test;
    size_t assertions_failed = checks_failed - checks_failed_before_test;

    // Print test summary
    if (assertions_failed == 0) {
      if (assertions_passed == 0) {
        std::print(std::cerr, "Failed: '{}' ({}:{}), no checks performed\n",
                   test.name, test.file, test.line);
        failed_tests.push_back(&test);
      } else {
        std::print(std::cerr, "Passed: '{}' ({} assertions) ({}:{})\n",
                   test.name, assertions_passed, test.file, test.line);
      }
    } else {
      std::print(std::cerr, "Failed: '{}' ({} assertions, {} failed) ({}:{})\n",
                 test.name, assertions_passed + assertions_failed,
                 assertions_failed, test.file, test.line);
      failed_tests.push_back(&test);
    }

    // Print detailed assertion info
    for (auto const &check : failed_checks) {
      std::print(std::cerr, "  Failed {}: {}:{}\n", check.type, check.file,
                 check.line);
    }
    failed_checks.clear();
  }

  if (failed_tests.size() != 0) {
    std::print(std::cerr, "Failed {}/{} tests (passed {}/{} assertions)\n",
               failed_tests.size(), tests_run, checks_passed,
               checks_passed + checks_failed);
    return EXIT_FAILURE;
  }

  assert(checks_failed == 0);
  std::print(std::cerr, "Passed {} tests ({} assertions)\n", tests_run,
             checks_passed);
  return EXIT_SUCCESS;
}
