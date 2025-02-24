CXX := g++
CXX_FLAGS := -std=c++20 -Wall -Wpedantic -Werror -Wextra -fsanitize=undefined -fsanitize=address

.phony: default
default: regex-jit.o

regex-jit.o: regex-jit.cpp
	$(CXX) $(CXX_FLAGS) -c $^ -o $@
