# M28 Language Test Suite

This directory contains a consolidated set of tests for the M28 language implementation. The tests are organized by functionality to make them more maintainable and easier to understand.

## Test Files

The test suite includes the following test files:

1. **core-types-test.m28**: Tests basic types and arithmetic operations
   - Variables and assignments
   - Numeric operations (addition, subtraction, multiplication, division)
   - Various data types (numbers, strings, booleans, etc.)

2. **control-flow-test.m28**: Tests control flow structures
   - Conditional statements (if, if-elif-else)
   - Loop constructs (for, while)
   - Early loop termination and skipping iterations

3. **data-structures-test.m28**: Tests data structure operations
   - List creation and manipulation
   - Dictionary operations
   - Collection iteration

4. **function-test.m28**: Tests function definitions and usage
   - Basic function definitions
   - Recursive functions
   - Higher-order functions
   - Function parameters and return values

5. **exception-test.m28**: Tests exception handling and traceback functionality
   - Basic exception handling (try/except/finally)
   - Custom exceptions
   - Nested exception handling
   - Traceback information for debugging

## Running the Tests

To run all tests:

```bash
cd /Users/mim/src/m28
./run-tests.sh
```

To run a specific test:

```bash
./bin/m28 tests/<test-file.m28>
```

## Archive

The `archive` directory contains:
- Previous versions of tests
- Debug files used during development
- Original test files before consolidation