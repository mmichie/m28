# CPython Test Suite - Quick Reference

This document helps you explore CPython's official test suite with M28.

## ⚠️  Important Note

**CPython's test suite uses `unittest`**, which M28 doesn't currently support. This means most tests won't run directly. However, you can:

1. **Read the tests** to understand what Python features they validate
2. **Extract test logic** to create simpler M28 versions (like we did in `tests/cpython/`)
3. **Use as reference** for what needs to be implemented
4. **Identify gaps** in M28's Python3 compatibility

## Setup

CPython tests are cloned locally (not committed to git):

```bash
# Already done:
cd tests
git clone --depth 1 https://github.com/python/cpython.git cpython-source
cd ..

# Added to .gitignore so it won't be committed
```

## How to Use CPython Tests

Since tests won't run directly, here's the recommended workflow:

### 1. Browse and Read Tests
```bash
# View a test file
cat tests/cpython-source/Lib/test/test_grammar.py | less
cat tests/cpython-source/Lib/test/test_list.py | less

# Search for specific features
grep -A 10 "def test_append" tests/cpython-source/Lib/test/test_list.py

# Count test methods
grep "def test_" tests/cpython-source/Lib/test/test_list.py | wc -l
```

### 2. Extract and Simplify
Look at the test logic and create a simpler version without `unittest`:

**CPython test (test_list.py):**
```python
class ListTest(unittest.TestCase):
    def test_append(self):
        a = []
        a.append(0)
        a.append(1)
        self.assertEqual(a, [0, 1])
```

**M28 version (your test):**
```python
# Test list append
a = []
a.append(0)
a.append(1)
assert a == [0, 1]
print("✓ List append works")
```

### 3. Identify Patterns
Read multiple test files to understand what features Python3 has that M28 might be missing.

## Browsing Tests

```bash
# List all test files
ls tests/cpython-source/Lib/test/test_*.py

# See test count
ls tests/cpython-source/Lib/test/test_*.py | wc -l  # 388 tests
```

## Recommended Tests (Start Here)

### ✅ Core Language (High Success Expected)
These test basic Python syntax that M28 implements well:

```bash
./run_cpython_test.sh grammar       # Literals, operators, statements
./run_cpython_test.sh builtin       # Built-in functions
./run_cpython_test.sh bool          # Boolean operations
./run_cpython_test.sh int           # Integer operations
./run_cpython_test.sh float         # Float operations
./run_cpython_test.sh string        # String methods
```

### ✅ Data Structures (High Success Expected)
```bash
./run_cpython_test.sh list          # List operations
./run_cpython_test.sh dict          # Dictionary operations
./run_cpython_test.sh set           # Set operations
./run_cpython_test.sh tuple         # Tuple behavior
```

### ⚠️  Control Flow (Partial Success Expected)
```bash
./run_cpython_test.sh if            # If statements
./run_cpython_test.sh while         # While loops
./run_cpython_test.sh for           # For loops
./run_cpython_test.sh with          # Context managers
./run_cpython_test.sh exceptions    # Exception handling
```

### ⚠️  Functions & OOP (Partial Success Expected)
```bash
./run_cpython_test.sh functools     # Function utilities
./run_cpython_test.sh decorators    # Decorator syntax
./run_cpython_test.sh class         # Class definitions
./run_cpython_test.sh descr         # Descriptors
```

### ⚠️  Advanced (Lower Success Expected)
```bash
./run_cpython_test.sh generators    # Generator functions
./run_cpython_test.sh itertools     # Iterator utilities
./run_cpython_test.sh comprehension # Comprehensions
```

### ❌ Will Likely Fail (CPython-specific)
These require CPython internals or C extensions:
```bash
./run_cpython_test.sh gc            # Garbage collection (refcount-specific)
./run_cpython_test.sh ctypes        # C foreign function interface
./run_cpython_test.sh threading     # Threading (not implemented)
./run_cpython_test.sh multiprocessing
./run_cpython_test.sh asyncio       # Async/await (partial)
```

## Understanding Test Results

### ✅ Success Indicators
- Test completes without errors
- Outputs match expected behavior
- All assertions pass

### ⚠️  Partial Success
- Some tests pass, some fail
- Core functionality works
- Edge cases or advanced features fail

### ❌ Failure Reasons
1. **Missing stdlib modules** - M28 doesn't include all Python stdlib
2. **CPython internals** - Tests checking refcounting, gc, etc.
3. **C extensions** - Tests requiring compiled extensions
4. **Implementation details** - Different error messages, stack traces
5. **Not implemented yet** - Features M28 hasn't added

## Useful Test Categories

### Quick Wins (Try These First)
```bash
./run_cpython_test.sh grammar
./run_cpython_test.sh list
./run_cpython_test.sh dict
./run_cpython_test.sh builtin
```

### Worth Exploring
```bash
./run_cpython_test.sh types
./run_cpython_test.sh operators
./run_cpython_test.sh itertools
./run_cpython_test.sh functools
```

### Ambitious (May need more work)
```bash
./run_cpython_test.sh class
./run_cpython_test.sh descr
./run_cpython_test.sh decorators
./run_cpython_test.sh generators
```

## Finding Specific Tests

```bash
# List all tests
ls tests/cpython-source/Lib/test/test_*.py | wc -l  # 388 tests

# Search for specific features
ls tests/cpython-source/Lib/test/test_*async*.py
ls tests/cpython_source/Lib/test/test_*class*.py
ls tests/cpython-source/Lib/test/test_*iter*.py

# View a test file
cat tests/cpython-source/Lib/test/test_grammar.py | head -50
```

## Tips for Success

1. **Start small** - Try core language tests first
2. **Expect failures** - CPython tests are comprehensive and test internals
3. **Look for patterns** - Partial passes show what works
4. **Check output** - Even failures give useful info about compatibility
5. **Focus on pure Python** - Avoid tests requiring C extensions

## Example Session

```bash
# Run a simple test
./run_cpython_test.sh grammar

# See what passed/failed
# Analyze the output to understand M28's capabilities

# Try another
./run_cpython_test.sh list

# Compare results
# Build a mental model of what works
```

## Next Steps

After exploring:
1. Identify patterns in what works/fails
2. Fix common issues
3. Add missing features based on failures
4. Create M28-specific versions of important tests
5. Eventually: integrate as full test suite (Option A from earlier)

## Location

- CPython source: `tests/cpython-source/`
- Test files: `tests/cpython-source/Lib/test/test_*.py`
- Standard library: `tests/cpython-source/Lib/`
- Not in git: Added to `.gitignore`
