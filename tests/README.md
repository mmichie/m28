# M28 Pythonic Lisp Test Suite

This directory contains a set of test files for the M28 Pythonic Lisp language. These tests are designed to verify the functionality of the interpreter by testing various language features.

## Test Files

1. **arithmetic-test.m28** - Tests arithmetic operations (+, -, *, /, %, **)
2. **variable-test.m28** - Tests variable assignment and different data types
3. **control-flow-test.m28** - Tests if/elif/else statements and loops
4. **list-test.m28** - Tests list creation, access, and operations

## Running Tests

You can run individual test files:

```bash
./bin/m28 tests/arithmetic-test.m28
```

Or run all tests at once using the provided script:

```bash
./run_tests.sh
```

## Test Format

Each test file follows a simple format:

1. Test output is designed to be human-readable
2. Each test prints what it's testing, the actual result, and the expected result
3. Tests for each language feature are grouped together

## Test Development Guidelines

When developing new tests:

1. Keep tests for different language features in separate files
2. Make test output easy to read and verify
3. Test both normal cases and edge cases
4. Test complex combinations of language features

## Common Test Patterns

The test files demonstrate these common patterns:

1. Testing simple expressions (`1 + 2`)
2. Testing operator precedence and complex expressions
3. Testing defined functions with various inputs
4. Testing control structures and flow
5. Testing data structures and operations

These tests can be extended as the language evolves to incorporate new features or to provide more comprehensive testing of existing features.