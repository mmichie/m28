#!/bin/bash
# Script to run all working tests

echo "===== Running m28 tests ====="
echo ""

# Run individual tests
TESTS=(
  "arithmetic-test.m28"
  "basic-test.m28"
  "control-flow-test.m28"
  "dict-test.m28"
  "function-test.m28"
  "list-test.m28"
  "variable-test.m28"
)

for test in "${TESTS[@]}"; do
  echo "Running tests/$test..."
  ./bin/m28 "tests/$test"
  echo "------------------------------------"
  echo ""
done

echo "===== All tests completed ====="