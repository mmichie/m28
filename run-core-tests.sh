#!/bin/bash
# Core test runner for M28 - runs only essential tests

echo "===== Running M28 Core Tests ====="
echo ""

# Essential tests only
TESTS=(
    "core-types-test.m28"
    "control-flow-test.m28"
    "function-test.m28"
    "exception-test.m28"
    "dot-notation-test.m28"
    "dict-test.m28"
)

# Make sure the app is built first
make build

PASSED=0
FAILED=0

# Run each test
for test in "${TESTS[@]}"; do
    echo "Running tests/$test..."
    if timeout 10s ./bin/m28 "tests/$test" 2>&1; then
        echo -e "✓ PASSED: $test\n"
        PASSED=$((PASSED + 1))
    else
        echo -e "✗ FAILED: $test\n"
        FAILED=$((FAILED + 1))
    fi
done

# Print summary
echo "===== Test Summary ====="
echo "Total tests: $((PASSED + FAILED))"
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [ $FAILED -eq 0 ]; then
    echo -e "\nAll core tests passed!"
    exit 0
else
    echo -e "\nSome tests failed."
    exit 1
fi