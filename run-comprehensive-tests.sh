#!/bin/bash

# M28 Comprehensive Test Runner
# Runs all comprehensive test files and reports overall status

# Set colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}===== Running M28 Comprehensive Tests =====${NC}"
echo

# Build M28 if bin directory doesn't exist
if [ ! -d "bin" ] || [ ! -f "bin/m28" ]; then
    echo -e "${YELLOW}Building M28...${NC}"
    make build
    echo
fi

# Tests to run - only the comprehensive test files
TESTS=(
    "tests/type-system-test-comprehensive.m28"
    "tests/tuple-test-comprehensive.m28"
    "tests/generator-test-comprehensive.m28"
    "tests/context-manager-test-comprehensive.m28"
    "tests/object-protocol-test-comprehensive.m28"
    "tests/concurrency-test-comprehensive.m28"
    "tests/performance-benchmark.m28"
    "tests/basic-module-test.m28"
    "tests/module-import-test.m28"
    "tests/repl-test.m28"
)

# Initialize counters
TOTAL=0
PASSED=0
FAILED=0

# Run each test
for test in "${TESTS[@]}"; do
    if [ -f "$test" ]; then
        echo -e "${YELLOW}Running ${test}...${NC}"
        
        # Run the test and capture output and exit code
        output=$(./bin/m28 "$test" 2>&1)
        exit_code=$?
        
        # Increment total count
        TOTAL=$((TOTAL + 1))
        
        # Check if test passed
        if [ $exit_code -eq 0 ]; then
            echo -e "${GREEN}PASSED: ${test}${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}FAILED: ${test}${NC}"
            echo "Error output:"
            echo "$output"
            FAILED=$((FAILED + 1))
        fi
        
        echo -e "------------------------------------\n"
    else
        echo -e "${RED}File not found: ${test}${NC}"
    fi
done

# Print summary
echo -e "${YELLOW}===== Test Summary =====${NC}"
echo -e "Total tests: ${TOTAL}"
echo -e "Passed: ${GREEN}${PASSED}${NC}"
echo -e "Failed: ${RED}${FAILED}${NC}"

# Print final message
if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All comprehensive tests passed successfully!${NC}"
    exit 0
else
    echo -e "\n${RED}${FAILED} test(s) failed. Please check the output above.${NC}"
    exit 1
fi