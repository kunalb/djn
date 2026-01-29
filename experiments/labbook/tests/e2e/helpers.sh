#!/bin/bash
# E2E test helpers for lab CLI

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Path to lab binary (built by cargo)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
LAB_BIN="${LAB_BIN:-$PROJECT_ROOT/target/debug/lab}"

# Temporary directory for test repos
TEST_DIR=""

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

setup_test_env() {
    TEST_DIR=$(mktemp -d)
    export HOME="$TEST_DIR/home"
    mkdir -p "$HOME"

    # Create source repo
    mkdir -p "$TEST_DIR/source"
    cd "$TEST_DIR/source"
    git init
    git config user.email "test@example.com"
    git config user.name "Test User"

    # Initial commit
    echo "# Test Project" > README.md
    git add README.md
    git commit -m "Initial commit"

    echo "Test environment set up in: $TEST_DIR"
}

cleanup_test_env() {
    if [ -n "$TEST_DIR" ] && [ -d "$TEST_DIR" ]; then
        rm -rf "$TEST_DIR"
    fi
}

# Run lab command
lab() {
    "$LAB_BIN" "$@"
}

# Assert that a command succeeds
assert_success() {
    local description="$1"
    shift

    TESTS_RUN=$((TESTS_RUN + 1))

    if "$@" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $description"
        echo "  Command: $*"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Assert that a command fails
assert_failure() {
    local description="$1"
    shift

    TESTS_RUN=$((TESTS_RUN + 1))

    if "$@" > /dev/null 2>&1; then
        echo -e "${RED}FAIL${NC}: $description (expected failure)"
        echo "  Command: $*"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    else
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    fi
}

# Assert that a file exists
assert_file_exists() {
    local description="$1"
    local filepath="$2"

    TESTS_RUN=$((TESTS_RUN + 1))

    if [ -f "$filepath" ]; then
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $description"
        echo "  File not found: $filepath"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Assert that a file contains a string
assert_file_contains() {
    local description="$1"
    local filepath="$2"
    local pattern="$3"

    TESTS_RUN=$((TESTS_RUN + 1))

    if grep -q "$pattern" "$filepath" 2>/dev/null; then
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $description"
        echo "  Pattern '$pattern' not found in: $filepath"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Assert that a file is empty
assert_file_empty() {
    local description="$1"
    local filepath="$2"

    TESTS_RUN=$((TESTS_RUN + 1))

    if [ -f "$filepath" ] && [ ! -s "$filepath" ]; then
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $description"
        echo "  File not empty or not found: $filepath"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Assert that a directory exists
assert_dir_exists() {
    local description="$1"
    local dirpath="$2"

    TESTS_RUN=$((TESTS_RUN + 1))

    if [ -d "$dirpath" ]; then
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $description"
        echo "  Directory not found: $dirpath"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Assert command output contains string
assert_output_contains() {
    local description="$1"
    local pattern="$2"
    shift 2

    TESTS_RUN=$((TESTS_RUN + 1))

    local output
    output=$("$@" 2>&1) || true

    if echo "$output" | grep -q "$pattern"; then
        echo -e "${GREEN}PASS${NC}: $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}: $description"
        echo "  Pattern '$pattern' not found in output"
        echo "  Output: $output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Print test summary
print_summary() {
    echo ""
    echo "=============================="
    echo "Test Summary"
    echo "=============================="
    echo "Total:  $TESTS_RUN"
    echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Failed: ${RED}$TESTS_FAILED${NC}"
    echo ""

    if [ "$TESTS_FAILED" -gt 0 ]; then
        return 1
    fi
    return 0
}
