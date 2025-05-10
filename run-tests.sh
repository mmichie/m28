#!/bin/bash
# Test runner for all M28 language tests

echo "===== Running M28 Tests ====="
echo ""

# Array of tests to run
TESTS=(
    "core-types-test.m28"
    "control-flow-test.m28"
    "data-structures-test.m28"
    "function-test.m28"
    "exception-test.m28"
    "dot-notation-test.m28"
)

PASSED=0
FAILED=0

# Run each test
for test in "${TESTS[@]}"; do
    echo "Running tests/$test..."
    if ./bin/m28 "tests/$test"; then
        echo -e "PASSED: $test\n------------------------------------\n"
        PASSED=$((PASSED + 1))
    else
        echo -e "FAILED: $test\n------------------------------------\n"
        FAILED=$((FAILED + 1))
    fi
done

# Print summary
echo "===== Test Summary ====="
echo "Total tests: $((PASSED + FAILED))"
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [ $FAILED -eq 0 ]; then
    echo -e "\nAll tests passed successfully!"
    exit 0
else
    echo -e "\nSome tests failed."
    exit 1
fi