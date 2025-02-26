CXX := g++
CXX_FLAGS := -Og -g -std=c++20 -Wall -Wpedantic -Werror -Wextra -fsanitize=undefined -fsanitize=address

.phony: default
default: regex-jit

regex-jit: regex-jit.cpp
	$(CXX) $(CXX_FLAGS) $^ -o $@
