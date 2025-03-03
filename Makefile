CXX := g++
AR := ar
CXX_FLAGS := -g -std=c++2b -Wall -Wpedantic -Werror -Wextra
CXX_FLAGS += -fsanitize=undefined -fsanitize=address
CXX_FLAGS += -Iinclude
CXX_FLAGS += -O0

BUILD_DIR := build

.PHONY: default
default: regex-jit build/test.out

TARGET := regex-jit
LIB_SOURCES := \
	src/engine.cpp \
	src/parser.cpp \
	src/visualize.cpp

LIB_OBJECTS = $(patsubst src/%, build/%, $(patsubst %.cpp, %.o, $(LIB_SOURCES)))
DEP_INCLUDES = $(patsubst build/%.o, build/%.d, $(LIB_OBJECTS))
STATIC_LIB = $(BUILD_DIR)/regex.a

TEST_SOURCES := \
	test/testing.cpp \
	test/test_matching.cpp

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

-include $(DEP_INCLUDES)
$(BUILD_DIR)/%.o: src/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) -c $^ -o $@

$(STATIC_LIB): $(LIB_OBJECTS)
	$(AR) r $(STATIC_LIB) $(LIB_OBJECTS)

-include build/test.d
build/test.out: $(TEST_SOURCES) $(STATIC_LIB)
	$(CXX) -MMD $(CXX_FLAGS) $(TEST_SOURCES) $(STATIC_LIB) -o build/test.out

regex-jit: regex-jit.cpp $(STATIC_LIB) $(wildcard include/regex/*.hpp)
	$(CXX) $(CXX_FLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -r regex-jit $(BUILD_DIR)
