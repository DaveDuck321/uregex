CXX := g++
AR := ar
OBJCOPY := objcopy
CXX_FLAGS := -g -std=c++26 -Wall -Wpedantic -Werror -Wextra
CXX_FLAGS += -fsanitize=undefined -fsanitize=address
CXX_FLAGS += -Iinclude
CXX_FLAGS += -O0

BUILD_DIR := build
UNICODE_CATEGORY_DATA := $(BUILD_DIR)/unicode.bin

.PHONY: default
default: regex-jit $(BUILD_DIR)/test.out

TARGET := regex-jit
LIB_SOURCES := \
	src/engine.cpp \
	src/parser.cpp \
	src/visualize.cpp \
	src/unicode.cpp

LIB_OBJECTS = $(patsubst src/%, $(BUILD_DIR)/%, $(patsubst %.cpp, %.o, $(LIB_SOURCES)))
DEP_INCLUDES = $(patsubst $(BUILD_DIR)/%.o, $(BUILD_DIR)/%.d, $(LIB_OBJECTS))
STATIC_LIB = $(BUILD_DIR)/regex.a

TEST_SOURCES := \
	test/testing.cpp \
	test/test_classes.cpp \
	test/test_matching.cpp \

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

-include $(DEP_INCLUDES)
$(BUILD_DIR)/%.o: src/%.cpp | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) -c $< -o $@

$(BUILD_DIR)/unicode.o: src/unicode.cpp $(UNICODE_CATEGORY_DATA) | $(BUILD_DIR)
	$(CXX) -MMD $(CXX_FLAGS) -c $< -o $@
	$(OBJCOPY) $@ --update-section unicode_category_data=$(UNICODE_CATEGORY_DATA)


$(STATIC_LIB): $(LIB_OBJECTS)
	$(AR) r $(STATIC_LIB) $(LIB_OBJECTS)

$(UNICODE_CATEGORY_DATA): unicode/pack_categories.py
	./unicode/pack_categories.py --version 16.0.0 --output $(UNICODE_CATEGORY_DATA)

-include $(BUILD_DIR)/test.d
$(BUILD_DIR)/test.out: $(TEST_SOURCES) $(STATIC_LIB)
	$(CXX) -MMD $(CXX_FLAGS) $(TEST_SOURCES) $(STATIC_LIB) -o $@

regex-jit: regex-jit.cpp $(STATIC_LIB) $(wildcard include/regex/*.hpp)
	$(CXX) $(CXX_FLAGS) $< $(STATIC_LIB) -o $@

.PHONY: clean
clean:
	rm -r regex-jit $(BUILD_DIR)
