# Makefile for RPG Runtime Library
# Copyright (c) 2026

CC = gcc
AR = ar
CFLAGS = -Wall -Wextra -O2 -fPIC -std=c11 -D_POSIX_C_SOURCE=200809L
LDFLAGS = -lm -lsqlite3

# Directories
SRC_DIR = .
BUILD_DIR = build
TEST_DIR = tests

# Library name
LIB_NAME = librpg_runtime
STATIC_LIB = $(BUILD_DIR)/$(LIB_NAME).a
SHARED_LIB = $(BUILD_DIR)/$(LIB_NAME).so

# Source files
SOURCES = rpg_runtime.c rpg_file.c rpg_data.c
OBJECTS = $(SOURCES:%.c=$(BUILD_DIR)/%.o)

# Test files
TEST_SOURCES = $(wildcard $(TEST_DIR)/*_test.c)
TEST_BINS = $(TEST_SOURCES:$(TEST_DIR)/%.c=$(BUILD_DIR)/%)

# Default target
all: directories $(STATIC_LIB) $(SHARED_LIB)

# Create build directory
directories:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(TEST_DIR)

# Build static library
$(STATIC_LIB): $(OBJECTS)
	@echo "Creating static library: $@"
	$(AR) rcs $@ $^

# Build shared library
$(SHARED_LIB): $(OBJECTS)
	@echo "Creating shared library: $@"
	$(CC) -shared -o $@ $^ $(LDFLAGS)

# Compile source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@echo "Compiling: $<"
	$(CC) $(CFLAGS) -c $< -o $@

# Build tests
tests: $(TEST_BINS)

$(BUILD_DIR)/%_test: $(TEST_DIR)/%_test.c $(STATIC_LIB)
	@echo "Building test: $@"
	$(CC) $(CFLAGS) $< -o $@ -L$(BUILD_DIR) -lrpg_runtime $(LDFLAGS)

# Run tests
test: tests
	@echo "Running tests..."
	@for test in $(TEST_BINS); do \
		echo "Running $$test..."; \
		$$test || exit 1; \
	done
	@echo "All tests passed!"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)

# Install library (requires sudo)
install: all
	@echo "Installing library to /usr/local/lib..."
	sudo cp $(STATIC_LIB) /usr/local/lib/
	sudo cp $(SHARED_LIB) /usr/local/lib/
	@echo "Installing header to /usr/local/include..."
	sudo cp rpg_runtime.h /usr/local/include/
	@echo "Running ldconfig..."
	sudo ldconfig

# Uninstall library (requires sudo)
uninstall:
	@echo "Uninstalling library..."
	sudo rm -f /usr/local/lib/$(LIB_NAME).a
	sudo rm -f /usr/local/lib/$(LIB_NAME).so
	sudo rm -f /usr/local/include/rpg_runtime.h
	sudo ldconfig

# Show help
help:
	@echo "RPG Runtime Library Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Build static and shared libraries (default)"
	@echo "  tests      - Build test programs"
	@echo "  test       - Build and run all tests"
	@echo "  clean      - Remove build artifacts"
	@echo "  install    - Install library system-wide (requires sudo)"
	@echo "  uninstall  - Remove installed library (requires sudo)"
	@echo "  help       - Show this help message"

.PHONY: all directories tests test clean install uninstall help