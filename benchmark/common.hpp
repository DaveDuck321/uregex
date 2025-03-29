#pragma once

#include <fcntl.h>
#include <filesystem>
#include <string_view>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

struct MappedFile {
  std::string_view underlying;

  explicit MappedFile(std::filesystem::path const &file) {
    int fd = ::open(file.c_str(), O_RDONLY);
    struct ::stat state_buffer = {};
    ::fstat(fd, &state_buffer);
    void *result =
        ::mmap(nullptr, state_buffer.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    ::close(fd);

    underlying = {(char const *)result, (size_t)state_buffer.st_size};
  }
  MappedFile(MappedFile const &) = delete;
  MappedFile(MappedFile &&other) : underlying{other.underlying} {
    other.underlying = {};
  }

  auto operator=(MappedFile const &) = delete;

  ~MappedFile() {
    if (underlying.size() != 0) {
      ::munmap((void *)underlying.data(), underlying.size());
    }
  }
};
