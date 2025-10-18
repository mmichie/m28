#!/bin/bash
# Run CPython test files against M28
# Usage: ./run_cpython_test.sh [test_name]
#   test_name: Name of test without test_ prefix or .py extension
#   Examples:
#     ./run_cpython_test.sh grammar
#     ./run_cpython_test.sh list
#     ./run_cpython_test.sh --list-all

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

M28_BIN="./bin/m28"
CPYTHON_TEST_DIR="tests/cpython-source/Lib/test"

if [[ ! -d "$CPYTHON_TEST_DIR" ]]; then
    echo -e "${RED}Error: CPython tests not found at $CPYTHON_TEST_DIR${NC}"
    echo "Run: cd tests && git clone --depth 1 https://github.com/python/cpython.git cpython-source"
    exit 1
fi

if [[ ! -f "$M28_BIN" ]]; then
    echo -e "${RED}Error: M28 binary not found. Run 'make build' first.${NC}"
    exit 1
fi

# List all available tests
if [[ "$1" == "--list-all" ]]; then
    echo -e "${BLUE}Available CPython tests (388 total):${NC}"
    ls $CPYTHON_TEST_DIR/test_*.py | sed 's/.*test_/  /' | sed 's/.py$//' | head -50
    echo "  ..."
    echo ""
    echo "Run with: ./run_cpython_test.sh <test_name>"
    exit 0
fi

# List recommended tests
if [[ "$1" == "--list" || -z "$1" ]]; then
    echo -e "${BLUE}Recommended CPython tests to try:${NC}"
    echo ""
    echo -e "${YELLOW}Core Language:${NC}"
    echo "  grammar       - Python grammar (literals, operators, statements)"
    echo "  builtin       - Built-in functions (len, sum, range, etc.)"
    echo "  types         - Built-in types behavior"
    echo ""
    echo -e "${YELLOW}Data Structures:${NC}"
    echo "  list          - List operations and methods"
    echo "  dict          - Dictionary operations"
    echo "  set           - Set operations"
    echo "  tuple         - Tuple behavior"
    echo "  string        - String methods"
    echo ""
    echo -e "${YELLOW}Control Flow:${NC}"
    echo "  if            - If statements"
    echo "  while         - While loops"
    echo "  for           - For loops"
    echo "  exceptions    - Exception handling"
    echo ""
    echo -e "${YELLOW}Functions & OOP:${NC}"
    echo "  functools     - Function utilities"
    echo "  decorators    - Decorator syntax"
    echo "  class         - Class definitions"
    echo "  descr         - Descriptors and protocols"
    echo ""
    echo -e "${YELLOW}Advanced:${NC}"
    echo "  generators    - Generator functions"
    echo "  itertools     - Iterator utilities"
    echo "  comprehension - List/dict/set comprehensions"
    echo ""
    echo "Usage: ./run_cpython_test.sh <test_name>"
    echo "List all: ./run_cpython_test.sh --list-all"
    exit 0
fi

# Run specific test
TEST_NAME="$1"
TEST_FILE="$CPYTHON_TEST_DIR/test_${TEST_NAME}.py"

if [[ ! -f "$TEST_FILE" ]]; then
    echo -e "${RED}Error: Test file not found: $TEST_FILE${NC}"
    echo "Run './run_cpython_test.sh --list' to see available tests"
    exit 1
fi

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}  Running CPython test: $TEST_NAME${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""
echo "Test file: $TEST_FILE"
echo "M28 binary: $M28_BIN"
echo ""
echo -e "${YELLOW}Note: This is CPython's test suite. Many tests may fail due to:${NC}"
echo "  - CPython-specific implementation details"
echo "  - Missing stdlib modules"
echo "  - Different error messages"
echo "  - C extension requirements"
echo ""
echo -e "${BLUE}Running test...${NC}"
echo "─────────────────────────────────────"

# Set PYTHONPATH to CPython's Lib directory
export PYTHONPATH="$CPYTHON_TEST_DIR/..:$PYTHONPATH"

# Run the test with timeout
if timeout 30s "$M28_BIN" "$TEST_FILE" 2>&1; then
    echo ""
    echo -e "${GREEN}✅ Test completed successfully!${NC}"
    exit 0
else
    EXIT_CODE=$?
    echo ""
    if [[ $EXIT_CODE -eq 124 ]]; then
        echo -e "${YELLOW}⏱ Test timed out after 30 seconds${NC}"
    else
        echo -e "${RED}✗ Test failed with exit code $EXIT_CODE${NC}"
    fi
    exit 1
fi
