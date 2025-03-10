CXX := g++
AR := ar
OBJCOPY := objcopy
CXX_FLAGS := -g -std=c++26 -Wall -Wpedantic -Werror -Wextra
CXX_FLAGS += -fsanitize=undefined -fsanitize=address
CXX_FLAGS += -Iinclude
CXX_FLAGS += -O0

BUILD_DIR := build

LIB_SOURCES := \
	src/engine.cpp \
	src/parser.cpp \
	src/visualize.cpp \
	src/unicode.cpp

LIB_OBJECTS = $(patsubst src/%, $(BUILD_DIR)/%, $(patsubst %.cpp, %.o, $(LIB_SOURCES)))
DEP_INCLUDES = $(patsubst $(BUILD_DIR)/%.o, $(BUILD_DIR)/%.d, $(LIB_OBJECTS))

TEST_SOURCES := \
	test/testing.cpp \
	test/test_classes.cpp \
	test/test_matching.cpp \
	test/test_groups.cpp \

.PHONY: default
default: regex-jit $(BUILD_DIR)/test.out

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

-include $(DEP_INCLUDES)
$(BUILD_DIR)/%.o: src/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) -c $< -o $@

$(BUILD_DIR)/unicode.o: src/unicode.cpp $(BUILD_DIR)/unicode.bin | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) -c $< -o $@
	$(OBJCOPY) $@ --update-section unicode_category_data=$(BUILD_DIR)/unicode.bin


$(BUILD_DIR)/regex.a: $(LIB_OBJECTS) | $(BUILD_DIR)
	$(AR) r $(BUILD_DIR)/regex.a $(LIB_OBJECTS)

$(BUILD_DIR)/unicode.bin: unicode/pack_categories.py | $(BUILD_DIR)
	./unicode/pack_categories.py --version 16.0.0 --output $(BUILD_DIR)/unicode.bin

-include $(BUILD_DIR)/test.d
$(BUILD_DIR)/test.out: $(TEST_SOURCES) $(BUILD_DIR)/regex.a | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(TEST_SOURCES) $(BUILD_DIR)/regex.a -o $@

regex-jit: regex-jit.cpp $(BUILD_DIR)/regex.a $(wildcard include/regex/*.hpp)
	$(CXX) $(CXX_FLAGS) $< $(BUILD_DIR)/regex.a -o $@

.PHONY: clean
clean:
	rm -r regex-jit $(BUILD_DIR)
