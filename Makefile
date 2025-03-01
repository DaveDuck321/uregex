CXX := g++
AR := ar
CXX_FLAGS := -O0 -g -std=c++2b -Wall -Wpedantic -Werror -Wextra -fsanitize=undefined -fsanitize=address

BUILD_DIR := build

.phony: default
default: regex-jit

TARGET := regex-jit
SOURCES := \
	src/engine.cpp \
	src/parser.cpp \
	src/visualize.cpp

LIB_OBJECTS = $(patsubst src/%, build/%, $(patsubst %.cpp, %.o, $(SOURCES)))
STATIC_LIB = $(BUILD_DIR)/regex.a

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.o: src/%.cpp | $(BUILD_DIR)
	$(CXX) $(CXX_FLAGS) -c $^ -o $@ -Iinclude

$(STATIC_LIB): $(LIB_OBJECTS)
	$(AR) r $(STATIC_LIB) $(LIB_OBJECTS)

regex-jit: regex-jit.cpp $(STATIC_LIB)
	$(CXX) $(CXX_FLAGS) -Iinclude $^ -o $@

.phony: clean
clean:
	rm -r regex-jit $(BUILD_DIR)
