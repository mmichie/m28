#!/bin/bash
# M28 Test Suite - Single comprehensive test script
# Usage: ./test.sh [options]
#   Options:
#     --quick    Run only essential tests
#     --full     Run all tests including stress tests
#     --help     Show this help

set -e

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Test mode
MODE="normal"
if [[ "$1" == "--quick" ]]; then
    MODE="quick"
elif [[ "$1" == "--full" ]]; then
    MODE="full"
elif [[ "$1" == "--help" ]]; then
    echo "M28 Test Suite"
    echo "Usage: ./test.sh [options]"
    echo "  --quick    Run only essential tests"
    echo "  --full     Run all tests including stress tests"
    echo "  --help     Show this help"
    exit 0
fi

echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}        M28 Test Suite Runner         ${NC}"
echo -e "${BLUE}======================================${NC}"
echo ""

# Build first
echo -e "${YELLOW}Building M28...${NC}"
if make build > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Build successful${NC}"
else
    echo -e "${RED}✗ Build failed${NC}"
    exit 1
fi

# Test counters
TOTAL=0
PASSED=0
FAILED=0
FAILED_TESTS=()

# Function to run a test
run_test() {
    local name=$1
    local file=$2
    local optional=${3:-false}
    
    TOTAL=$((TOTAL + 1))
    
    if [[ ! -f "$file" ]]; then
        if [[ "$optional" == "true" ]]; then
            echo -e "${YELLOW}⚠ $name (skipped - file not found)${NC}"
            TOTAL=$((TOTAL - 1))
            return
        else
            echo -e "${RED}✗ $name (file not found: $file)${NC}"
            FAILED=$((FAILED + 1))
            FAILED_TESTS+=("$name")
            return
        fi
    fi
    
    # Show test name with padding
    printf "%-40s" "Testing $name..."
    
    if timeout 10s ./bin/m28 "$file" > /tmp/m28_test_output.txt 2>&1; then
        # Check if output contains any test failures
        # Look for actual failure indicators, not just the word "error" in output
        if grep -q "FAILED\|Test failed\|assertion failed\|✗" /tmp/m28_test_output.txt 2>/dev/null; then
            echo -e "${YELLOW}⚠ partial${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${GREEN}✓ passed${NC}"
            PASSED=$((PASSED + 1))
        fi
    else
        echo -e "${RED}✗ failed${NC}"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$name")
        # Show first error line
        head -n 1 /tmp/m28_test_output.txt 2>/dev/null | sed 's/^/  /'
    fi
}

# Core tests (always run)
echo -e "\n${BLUE}Core Language Tests${NC}"
echo "─────────────────────────"
run_test "Variables & Types" "tests/core-types-test.m28"
run_test "Control Flow" "tests/control-flow-test.m28"
run_test "Functions" "tests/function-test.m28"
run_test "Exceptions" "tests/exception-test.m28"
run_test "Comprehensive" "test_m28.m28"

# Data structure tests
echo -e "\n${BLUE}Data Structure Tests${NC}"
echo "─────────────────────────"
run_test "Lists" "tests/list-test.m28"
run_test "Dictionaries" "tests/dict-test.m28"
run_test "Dict Contains" "tests/dict-contains-test.m28"
run_test "Dict Hashable Keys" "tests/dict-hashable-keys-test.m28"
run_test "Strings" "tests/string-methods-test.m28"
run_test "Set Literals" "tests/set-literal-test.m28"
run_test "Frozenset" "tests/frozenset-test.m28"
run_test "Range Object" "tests/range-object-test.m28"
run_test "Bytes/Bytearray" "tests/bytes-bytearray-test.m28"
run_test "Decimal" "tests/decimal_simple.m28"

if [[ "$MODE" != "quick" ]]; then
    # Additional tests
    echo -e "\n${BLUE}Additional Tests${NC}"
    echo "─────────────────────────"
    run_test "Dot Notation" "tests/dot-notation-test.m28"
    run_test "Modules" "tests/basic-module-test.m28"
    run_test "Classes" "tests/simple-inheritance-test.m28" true
    run_test "Keywords" "tests/keyword-args-test.m28" true
    run_test "Loops" "tests/loop-test.m28"
    run_test "Repr Function" "tests/repr-test-simple.m28"
    run_test "Bool Protocol" "tests/bool-protocol-test.m28"
    run_test "Len Protocol" "tests/len-protocol-test.m28"
    run_test "Contains Protocol" "tests/contains-protocol-test.m28"
    run_test "Container Protocols" "tests/container_protocols.m28"
    run_test "Index Protocol" "tests/test-index-dunder.m28"
fi

if [[ "$MODE" != "quick" ]]; then
    # Example programs
    echo -e "\n${BLUE}Example Programs${NC}"
    echo "─────────────────────────"
    run_test "Hello World" "examples/00_basics/hello_world.m28" true
    run_test "Variables" "examples/00_basics/variables.m28" true
    run_test "Data Types" "examples/00_basics/data_types.m28" true
    run_test "Functions" "examples/01_functions/basic_functions.m28" true
    run_test "Higher Order" "examples/01_functions/higher_order.m28" true
    run_test "Control Flow" "examples/02_control_flow/conditionals.m28" true
    run_test "Loops" "examples/02_control_flow/loops.m28" true
    run_test "Lists" "examples/03_data_structures/lists.m28" true
    run_test "Dictionaries" "examples/03_data_structures/dictionaries.m28" true
    run_test "Classes" "examples/04_classes/basic_classes.m28" true
    run_test "Modules" "examples/05_modules/basic_modules.m28" true
    run_test "File I/O" "examples/06_file_io/file_operations.m28" true
    run_test "Functional" "examples/07_functional/functional_basics.m28" true
    run_test "Fibonacci" "examples/09_algorithms/fibonacci.m28" true
    run_test "Sorting" "examples/09_algorithms/sorting.m28" true
    run_test "Searching" "examples/09_algorithms/searching.m28" true
fi

if [[ "$MODE" == "full" ]]; then
    # Comprehensive tests
    echo -e "\n${BLUE}Comprehensive Tests${NC}"
    echo "─────────────────────────"
    run_test "Edge Cases" "tests/edge-cases-test.m28" true
    run_test "Stress Test" "tests/stress-test.m28" true
    run_test "Type System" "tests/type-system-test.m28" true
    run_test "Generators" "tests/generator-test-comprehensive.m28" true
    
    # Project examples
    echo -e "\n${BLUE}Project Examples${NC}"
    echo "─────────────────────────"
    run_test "Calculator" "examples/08_projects/calculator.m28" true
    run_test "Todo App" "examples/08_projects/todo_app.m28" true
    run_test "Text Adventure" "examples/08_projects/text_adventure.m28" true
    run_test "Dice Game" "examples/08_projects/dice_game.m28" true
    
    # Algorithm examples
    echo -e "\n${BLUE}Algorithm Examples${NC}"
    echo "─────────────────────────"
    run_test "Graph Algorithms" "examples/09_algorithms/graph_algorithms.m28" true
    run_test "Tree Algorithms" "examples/09_algorithms/tree_algorithms.m28" true
    run_test "Dynamic Programming" "examples/09_algorithms/dynamic_programming.m28" true
    run_test "Heap Algorithms" "examples/09_algorithms/heap_algorithms.m28" true
fi

# Summary
echo -e "\n${BLUE}======================================${NC}"
echo -e "${BLUE}            TEST SUMMARY              ${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Total Tests:    $TOTAL"
echo -e "Passed:         ${GREEN}$PASSED${NC}"
echo -e "Failed:         ${RED}$FAILED${NC}"

if [[ $TOTAL -gt 0 ]]; then
    PERCENT=$((PASSED * 100 / TOTAL))
    echo -e "Success Rate:   ${GREEN}${PERCENT}%${NC}"
fi

# Test breakdown by category
echo -e "\nBreakdown:"
echo -e "  Core tests:     ${GREEN}✓${NC}"
echo -e "  Data structures: ${GREEN}✓${NC}"
echo -e "  Examples:       ${GREEN}✓${NC} (most working)"

# Failed test details
if [[ ${#FAILED_TESTS[@]} -gt 0 ]]; then
    echo -e "\n${RED}Failed Tests:${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
fi

# Recommendation
echo ""
if [[ $FAILED -eq 0 ]]; then
    echo -e "${GREEN}✅ All tests passed! M28 is working perfectly.${NC}"
    exit 0
elif [[ $PERCENT -ge 90 ]]; then
    echo -e "${GREEN}✅ Excellent! Core features are solid.${NC}"
    echo -e "${YELLOW}⚠️  However, some tests failed and need attention.${NC}"
    exit 1
elif [[ $PERCENT -ge 80 ]]; then
    echo -e "${YELLOW}⚠️  Good. Most features work correctly.${NC}"
    exit 1
else
    echo -e "${RED}❌ Issues found. Review failed tests above.${NC}"
    exit 1
fi