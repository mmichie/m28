# M28 Testing Guide

## Overview

The M28 test suite provides comprehensive testing for all language features, built-in functions, and advanced capabilities. Tests are organized into modules that can be run individually or as a complete suite.

## Test Structure

```
tests/
├── test_framework.m28      # Core testing utilities and assertions
├── test_core.m28          # Core language features
├── test_builtins.m28      # Built-in functions
├── test_special_forms.m28 # Special forms (if, def, lambda, etc.)
├── test_advanced.m28      # Advanced features (classes, async, etc.)
├── benchmarks.m28         # Performance benchmarks
├── run_tests.m28          # Main test runner
└── test_runner_simple.m28 # Simple standalone tests
```

## Running Tests

### Run All Tests
```bash
./run-m28-tests.sh
```

### Run Individual Test Modules
```bash
# Set module path
export M28_PATH="./tests:."

# Run specific test module
./bin/m28 -c "(import \"test_framework\") (import \"test_core\") (test-core)"
```

### Run Benchmarks
```bash
./run-m28-tests.sh --bench
```

### Run Memory Tests
```bash
./run-m28-tests.sh --memory
```

## Test Framework

The test framework provides these assertion functions:

- `assert` - Basic assertion with message
- `assert-eq` - Assert equality 
- `assert-neq` - Assert inequality
- `assert-true` - Assert value is true
- `assert-false` - Assert value is false
- `assert-nil` - Assert value is nil
- `assert-not-nil` - Assert value is not nil
- `assert-type` - Assert value has expected type
- `assert-error` - Assert code throws an error
- `assert-no-error` - Assert code runs without error

### Writing Tests

```lisp
(test/test-suite "My Test Suite" (lambda ()
  
  (test/run-test "Basic arithmetic" (lambda ()
    (test/assert-eq 4 (+ 2 2) "2 + 2 = 4")
    (test/assert-eq 10 (* 2 5) "2 * 5 = 10")))
  
  (test/run-test "String operations" (lambda ()
    (test/assert-eq "hello world" 
      (+ "hello" " " "world") 
      "String concatenation")))
))
```

## Test Coverage

### Core Features
- Numbers, strings, booleans, nil
- Lists, tuples, dictionaries, sets
- Variables and functions
- Control flow (if, cond, loops)
- Exception handling

### Built-in Functions
- Arithmetic operations
- String manipulation
- Type checking and conversion
- Sequence operations
- Functional programming (map, filter, reduce)
- I/O operations

### Special Forms
- Definition forms (def, set!)
- Quote and eval
- Control flow forms
- Let bindings
- Loop forms with break/continue
- Exception handling (try/except/finally)
- Lambda and function forms

### Advanced Features
- Module system (import/export)
- Class system with inheritance
- Generators with yield
- Context managers (with statement)
- Async/concurrent operations
- Object protocol (getattr, setattr)

## Performance Benchmarks

The benchmark suite measures:
- Arithmetic operations
- List and dictionary operations
- Function calls (recursive and iterative)
- String operations
- Higher-order functions
- Object creation and method calls
- Async task operations

## Known Limitations

Some tests may need adjustment based on the current implementation status:
- Exception handling tests may need modification
- Timing functions for benchmarks need builtin support
- Some advanced features may not be fully implemented

## Adding New Tests

1. Create a new test module following the naming convention `test_*.m28`
2. Import the test framework
3. Define test functions using the framework assertions
4. Add the module to `run_tests.m28`
5. Update this documentation

## Continuous Integration

Tests should be run:
- Before committing changes
- As part of CI/CD pipeline
- When adding new features
- After bug fixes