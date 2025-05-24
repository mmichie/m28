#!/bin/bash

# M28 Test Runner Script

set -e

echo "Building M28..."
make build

echo -e "\nRunning M28 tests..."

# Set module path to include tests directory
export M28_PATH="./tests:./examples:."

# Run the main test suite
echo -e "\n=== Running Test Suite ==="
./bin/m28 tests/run_tests.m28

# Run individual test files if main runner fails
if [ $? -ne 0 ]; then
    echo -e "\nMain test runner failed. Running individual test files..."
    
    echo -e "\n=== Testing Core ==="
    ./bin/m28 -c "(import \"tests/test_framework\") (import \"tests/test_core\") (test-core)"
    
    echo -e "\n=== Testing Builtins ==="
    ./bin/m28 -c "(import \"tests/test_framework\") (import \"tests/test_builtins\") (test-builtins)"
    
    echo -e "\n=== Testing Special Forms ==="
    ./bin/m28 -c "(import \"tests/test_framework\") (import \"tests/test_special_forms\") (test-special-forms)"
    
    echo -e "\n=== Testing Advanced Features ==="
    ./bin/m28 -c "(import \"tests/test_framework\") (import \"tests/test_advanced\") (test-advanced)"
fi

# Run benchmarks if requested
if [ "$1" = "--bench" ]; then
    echo -e "\n=== Running Benchmarks ==="
    ./bin/m28 -c "(import \"tests/benchmarks\") (run-benchmarks)"
fi

# Run memory test if requested
if [ "$1" = "--memory" ]; then
    echo -e "\n=== Running Memory Test ==="
    ./bin/m28 -c "(import \"tests/benchmarks\") (memory-test)"
fi

echo -e "\nTest run complete!"