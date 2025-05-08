#!/bin/bash
# Script to run all consolidated tests

echo "===== Running m28 tests ====="
echo ""

# Run consolidated tests
TESTS=(
  "core-types-test.m28"
  "control-flow-test.m28"
  "data-structures-test.m28"
  "function-test.m28"
  "exception-test.m28"
)

failed_tests=()

for test in "${TESTS[@]}"; do
  echo "Running tests/$test..."
  if ./bin/m28 "tests/$test"; then
    echo "PASSED: $test"
  else
    echo "FAILED: $test"
    failed_tests+=("$test")
  fi
  echo "------------------------------------"
  echo ""
done

# Print summary
echo "===== Test Summary ====="
echo "Total tests: ${#TESTS[@]}"
echo "Passed: $((${#TESTS[@]} - ${#failed_tests[@]}))"
echo "Failed: ${#failed_tests[@]}"

if [ ${#failed_tests[@]} -gt 0 ]; then
  echo "Failed tests:"
  for test in "${failed_tests[@]}"; do
    echo "- $test"
  done
  exit 1
else
  echo "All tests passed!"
fi

echo "===== All tests completed ====="