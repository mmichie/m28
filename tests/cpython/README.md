# CPython Test Suite - Curated for M28

This directory contains curated tests from CPython's official test suite to validate M28's Python3 compatibility.

## Approach

Instead of running the entire CPython test suite, we've selected ~50 high-value, pure-Python tests that validate core language features. This gives us:

- Fast feedback on Python3 compliance
- Systematic validation against official Python tests
- Clear tracking of what's implemented vs. missing

## Test Organization

Tests are organized by feature category:

### Tier 1: Core Language (Priority 1)
- `test_grammar.py` - Python grammar compliance
- `test_types.py` - Built-in types behavior
- `test_builtin.py` - Built-in functions
- `test_operations.py` - Basic operations

### Tier 2: Data Structures
- `test_list.py` - List operations and methods
- `test_dict.py` - Dictionary operations
- `test_set.py` - Set operations
- `test_tuple.py` - Tuple behavior
- `test_string.py` - String methods

### Tier 3: Control Flow
- `test_if.py` - Conditional statements
- `test_while.py` - While loops
- `test_for.py` - For loops
- `test_exceptions.py` - Exception handling

### Tier 4: Functions & OOP
- `test_functools.py` - Function utilities
- `test_decorators.py` - Decorator syntax
- `test_class.py` - Class definitions
- `test_descr.py` - Descriptors and protocols

### Tier 5: Advanced
- `test_generators.py` - Generator functions
- `test_itertools.py` - Iterator utilities
- `test_comprehensions.py` - List/dict/set comprehensions
- `test_contextlib.py` - Context managers

## Running Tests

```bash
# Run all CPython tests
./test.sh --cpython

# Run specific tier
./test.sh --cpython-tier 1

# Run specific test
./bin/m28 tests/cpython/test_grammar.py
```

## Status Tracking

See `conformance.json` for current pass/fail status of each test.

## Sources

Tests are adapted from:
- CPython 3.12 (https://github.com/python/cpython/tree/v3.12.6/Lib/test)
- Simplified to focus on language features, not implementation details
- Modified to remove CPython-specific assumptions (refcounting, gc, etc.)

## Expansion

As M28 implements more features, add tests from CPython's Lib/test/ directory following the same pattern.
