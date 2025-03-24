#include "private/jit.hpp"

#include <cstdint>
#include <ctime>
#include <elf.h>
#include <fcntl.h>
#include <iostream>
#include <mutex>
#include <sys/mman.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

using namespace uregex::jit;

// https://github.com/torvalds/linux/blob/master/tools/perf/Documentation/jitdump-specification.txt
struct [[gnu::packed]] JITDumpHeader {
  uint32_t magic;
  uint32_t version;
  uint32_t total_size;
  uint32_t elf_mach;
  uint32_t pad1;
  uint32_t pid;
  uint64_t timestamp;
  uint64_t flags;
};

struct [[gnu::packed]] JITRecordHeader {
  enum class ID : uint32_t {
    JIT_CODE_LOAD = 0,
    JIT_CODE_MOVE = 1,
    JIT_CODE_DEBUG_INFO = 2,
    JIT_CODE_CLOSE = 3,
    JIT_CODE_UNWINDING_INFO = 4,
  };

  ID id;
  uint32_t total_size; // sizeof(JITRecordHeader) + sizeof(Body)
  uint64_t timestamp;
};

struct [[gnu::packed]] JITCodeLoadRecordBody {
  uint32_t pid;
  uint32_t tid;
  uint64_t vma;
  uint64_t code_addr;
  uint64_t code_size;
  uint64_t code_index;
};

namespace {
auto get_timestamp() -> uint64_t {
  timespec time_out;
  auto err = ::clock_gettime(CLOCK_MONOTONIC, &time_out);
  assert(err == 0);

  uint64_t seconds = time_out.tv_sec;
  uint64_t nanoseconds = time_out.tv_nsec;
  std::cout << seconds << ", " << nanoseconds << std::endl;
  return seconds * 1'000'000'000UL + nanoseconds;
}

auto checked_write(int fd, void const *data, size_t size) -> void {
  [[maybe_unused]] uint64_t result = ::write(fd, data, size);
  assert(result == size);
}
} // namespace

JITDumper::JITDumper() : m_regex_index{0} {
  auto const jitdump_filename = std::format("./jit-{}.dump", ::getpid());
  std::lock_guard _{m_jitdump_body};
  m_fd = ::open(jitdump_filename.c_str(), O_CREAT | O_TRUNC | O_RDWR, 0666);
  // Hacky mmap required for perf to identify which file is the jitdump
  m_mapped_region =
      ::mmap(nullptr, 4096, PROT_EXEC | PROT_READ, MAP_PRIVATE, m_fd, 0);

  JITDumpHeader header{
      .magic = 0x4A695444U,
      .version = 1,
      .total_size = sizeof(JITDumpHeader),
      .elf_mach = EM_X86_64,
      .pad1 = 0,
      .pid = (uint32_t)::getpid(),
      .timestamp = get_timestamp(),
      .flags = 0,
  };

  checked_write(m_fd, &header, sizeof(JITDumpHeader));
}

JITDumper::~JITDumper() {
  ::munmap(m_mapped_region, 4096);
  ::close(m_fd);
}

auto JITDumper::record_load(std::span<uint8_t const> data, void *loaded_to)
    -> void {
  std::lock_guard _{m_jitdump_body};
  auto const function_name = std::format("regex_evaluator_{}", m_regex_index);
  JITRecordHeader header{
      .id = JITRecordHeader::ID::JIT_CODE_LOAD,
      .total_size =
          (uint32_t)(sizeof(JITRecordHeader) + sizeof(JITCodeLoadRecordBody) +
                     function_name.size() + 1 + data.size()),
      .timestamp = get_timestamp(),
  };
  JITCodeLoadRecordBody body{
      .pid = (uint32_t)::getpid(),
      .tid = (uint32_t)::gettid(),
      .vma = 0,
      .code_addr = (uint64_t)loaded_to,
      .code_size = data.size(),
      .code_index = m_regex_index,
  };

  checked_write(m_fd, &header, sizeof(JITRecordHeader));
  checked_write(m_fd, &body, sizeof(JITCodeLoadRecordBody));
  checked_write(m_fd, function_name.c_str(), function_name.size() + 1);
  checked_write(m_fd, data.data(), data.size());

  m_regex_index += 1;
}
