#!/bin/bash
# Run curated CPython tests for M28
# Usage: ./run_cpython_tests.sh [test_name]

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Find M28 binary
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
M28_BIN="$SCRIPT_DIR/../../bin/m28"

if [[ ! -f "$M28_BIN" ]]; then
    echo -e "${RED}Error: M28 binary not found at $M28_BIN${NC}"
    echo "Please run 'make build' first"
    exit 1
fi

# Test directory
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}   CPython Test Suite for M28        ${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""

# Test counters
TOTAL=0
PASSED=0
FAILED=0
FAILED_TESTS=()

# Function to run a test
run_test() {
    local test_file=$1
    local test_name=$(basename "$test_file" .py)

    TOTAL=$((TOTAL + 1))

    # Show test name with padding
    printf "%-40s" "Testing $test_name..."

    if timeout 30s "$M28_BIN" "$test_file" > /tmp/m28_cpython_test.txt 2>&1; then
        echo -e "${GREEN}✓ passed${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}✗ failed${NC}"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$test_name")
        # Show first error line
        head -n 5 /tmp/m28_cpython_test.txt 2>/dev/null | sed 's/^/  /'
    fi
}

# If specific test provided, run only that
if [[ -n "$1" ]]; then
    if [[ -f "$TEST_DIR/$1.py" ]]; then
        run_test "$TEST_DIR/$1.py"
    elif [[ -f "$TEST_DIR/$1" ]]; then
        run_test "$TEST_DIR/$1"
    else
        echo -e "${RED}Test file not found: $1${NC}"
        exit 1
    fi
else
    # Run all tests in priority order
    echo -e "${BLUE}Tier 1: Core Language${NC}"
    echo "─────────────────────────"
    [[ -f "$TEST_DIR/test_grammar.py" ]] && run_test "$TEST_DIR/test_grammar.py"
    [[ -f "$TEST_DIR/test_builtin.py" ]] && run_test "$TEST_DIR/test_builtin.py"

    echo ""
    echo -e "${BLUE}Tier 2: Data Structures${NC}"
    echo "─────────────────────────"
    [[ -f "$TEST_DIR/test_list.py" ]] && run_test "$TEST_DIR/test_list.py"
    [[ -f "$TEST_DIR/test_dict.py" ]] && run_test "$TEST_DIR/test_dict.py"
    [[ -f "$TEST_DIR/test_set.py" ]] && run_test "$TEST_DIR/test_set.py"
    [[ -f "$TEST_DIR/test_tuple.py" ]] && run_test "$TEST_DIR/test_tuple.py"
    [[ -f "$TEST_DIR/test_string.py" ]] && run_test "$TEST_DIR/test_string.py"
fi

# Summary
echo ""
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}         CPython Test Summary         ${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Total Tests:    $TOTAL"
echo -e "Passed:         ${GREEN}$PASSED${NC}"
echo -e "Failed:         ${RED}$FAILED${NC}"

if [[ $TOTAL -gt 0 ]]; then
    PERCENT=$((PASSED * 100 / TOTAL))
    echo -e "Success Rate:   ${GREEN}${PERCENT}%${NC}"
fi

# Failed test details
if [[ ${#FAILED_TESTS[@]} -gt 0 ]]; then
    echo ""
    echo -e "${RED}Failed Tests:${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
fi

# Exit code
if [[ $FAILED -eq 0 ]]; then
    echo ""
    echo -e "${GREEN}✅ All CPython tests passed!${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}❌ Some tests failed.${NC}"
    exit 1
fi
