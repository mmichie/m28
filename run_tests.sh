#!/bin/bash

echo "======================================"
echo "Running all M28 test suites"
echo "======================================"

echo -e "\n=== Running arithmetic-test.m28 ==="
./bin/m28 tests/arithmetic-test.m28

echo -e "\n=== Running variable-test.m28 ==="
./bin/m28 tests/variable-test.m28

echo -e "\n=== Running control-flow-test.m28 ==="
./bin/m28 tests/control-flow-test.m28

echo -e "\n=== Running list-test.m28 ==="
./bin/m28 tests/list-test.m28

echo -e "\n======================================"
echo "All tests completed"
echo "======================================"