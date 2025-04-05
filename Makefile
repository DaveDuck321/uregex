CXX := g++
AR := ar
OBJCOPY := objcopy
CXX_FLAGS := -g -std=c++26 -Wall -Wpedantic -Werror -Wextra
CXX_FLAGS += -Iinclude

DEBUG_CXX_FLAGS += -O0
RELEASE_CXX_FLAGS += -O3
LIB_CXX_FLAGS += -Isrc/

BUILD_DIR := build

LIB_SOURCES = \
	src/engine.cpp \
	src/graph_analyser.cpp \
	src/jit.cpp \
	src/jitdump.cpp \
	src/parser.cpp \
	src/regex.cpp \
	src/unicode.cpp \
	src/visualize.cpp \

LIB_DEBUG_OBJECTS = $(patsubst src/%, $(BUILD_DIR)/%, $(patsubst %.cpp, %.O0.o, $(LIB_SOURCES)))
LIB_RELEASE_OBJECTS = $(patsubst src/%, $(BUILD_DIR)/%, $(patsubst %.cpp, %.O3.o, $(LIB_SOURCES)))

DEP_INCLUDES = $(patsubst $(BUILD_DIR)/%.o, $(BUILD_DIR)/%.d, $(LIB_DEBUG_OBJECTS) $(LIB_RELEASE_OBJECTS))

TEST_SOURCES = \
	test/testing.cpp \
	test/test_classes.cpp \
	test/test_matching.cpp \
	test/test_groups.cpp \

TEST_OBJECTS = $(patsubst test/%.cpp, $(BUILD_DIR)/%.O0.o, $(TEST_SOURCES))
TEST_DEP_INCLUDES = $(patsubst $(BUILD_DIR)/%.o, $(BUILD_DIR)/%.d, $(TEST_OBJECTS))

.PHONY: default
default: regex-jit regex-jit-O3 $(BUILD_DIR)/test.out $(BUILD_DIR)/libregex.O0.a $(BUILD_DIR)/libregex.O3.a

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

-include $(DEP_INCLUDES)
$(BUILD_DIR)/%.O0.o: src/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) $(LIB_CXX_FLAGS) -c $< -o $@

$(BUILD_DIR)/%.O3.o: src/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) $(LIB_CXX_FLAGS) -c $< -o $@

$(BUILD_DIR)/unicode.O0.o: src/unicode.cpp $(BUILD_DIR)/unicode.bin | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) $(LIB_CXX_FLAGS) -c $< -o $@
	$(OBJCOPY) $@ --update-section unicode_category_data=$(BUILD_DIR)/unicode.bin

$(BUILD_DIR)/unicode.O3.o: src/unicode.cpp $(BUILD_DIR)/unicode.bin | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) $(LIB_CXX_FLAGS) -c $< -o $@
	$(OBJCOPY) $@ --update-section unicode_category_data=$(BUILD_DIR)/unicode.bin

$(BUILD_DIR)/libregex.O0.a: $(LIB_DEBUG_OBJECTS)
	$(AR) r $@ $^

$(BUILD_DIR)/libregex.O3.a: $(LIB_RELEASE_OBJECTS)
	$(AR) r $@ $^

$(BUILD_DIR)/unicode.bin: unicode/pack_categories.py | $(BUILD_DIR)
	./unicode/pack_categories.py --version 16.0.0 --output $(BUILD_DIR)/unicode.bin

-include $(TEST_DEP_INCLUDES)
$(BUILD_DIR)/%.O0.o: test/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) -c $< -o $@

$(BUILD_DIR)/%.O3.o: test/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) -c $< -o $@

$(BUILD_DIR)/test.out: $(TEST_OBJECTS) $(BUILD_DIR)/libregex.O0.a | $(BUILD_DIR)
	$(CXX) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) $^ -o $@

regex-jit: regex-jit.cpp $(BUILD_DIR)/libregex.O0.a $(wildcard include/regex/*.hpp)
	$(CXX) $(CXX_FLAGS) $(DEBUG_CXX_FLAGS) $< $(BUILD_DIR)/libregex.O0.a -o $@

regex-jit-O3: regex-jit.cpp $(BUILD_DIR)/libregex.O3.a $(wildcard include/regex/*.hpp)
	$(CXX) $(CXX_FLAGS) $(RELEASE_CXX_FLAGS) $< $(BUILD_DIR)/libregex.O3.a -o $@

.PHONY: clean
clean:
	rm -r regex-jit $(BUILD_DIR)
