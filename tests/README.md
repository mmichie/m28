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

3. **function-test.m28**: Tests function definitions and usage
   - Basic function definitions
   - Recursive functions
   - Higher-order functions
   - Function parameters and return values

4. **exception-test.m28**: Tests exception handling and traceback functionality
   - Basic exception handling (try/except/finally)
   - Custom exceptions
   - Nested exception handling
   - Traceback information for debugging

5. **Dictionary Tests**: Consolidated into a single test file
   - **dict-test.m28**: Tests all essential dictionary features:
     - Empty dictionaries
     - String and numeric keys
     - Nested dictionaries
     - Traditional dictionary creation with dict function
     - Accessing dictionary values

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

To run a specific test:

```bash
./bin/m28 tests/dict-test.m28
```