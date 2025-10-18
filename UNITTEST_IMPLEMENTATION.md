# Implementing unittest Support in M28

## Overview

To run CPython's test suite directly, M28 needs a `unittest` module implementation. Here's what it would take:

## Current Status ✅

M28 already has:
- ✅ Classes and inheritance
- ✅ Methods and `self`
- ✅ `assert` statement (we just added it!)
- ✅ Exception handling (try/except)
- ✅ Module system (import/from)

## What's Needed

### Phase 1: Core unittest.TestCase (Minimum Viable)

**Estimated effort: 2-4 hours**

Create `modules/unittest.m28` with:

```python
# modules/unittest.m28

class TestCase:
    """Base class for test cases"""

    def __init__(self):
        self._testMethodName = None

    # Setup/teardown hooks
    def setUp(self):
        pass

    def tearDown(self):
        pass

    # Essential assertions (start with these 10)
    def assertEqual(self, first, second, msg=None):
        if first != second:
            message = msg or f"{first} != {second}"
            raise AssertionError(message)

    def assertNotEqual(self, first, second, msg=None):
        if first == second:
            message = msg or f"{first} == {second}"
            raise AssertionError(message)

    def assertTrue(self, expr, msg=None):
        if not expr:
            message = msg or f"Expected True, got {expr}"
            raise AssertionError(message)

    def assertFalse(self, expr, msg=None):
        if expr:
            message = msg or f"Expected False, got {expr}"
            raise AssertionError(message)

    def assertIs(self, first, second, msg=None):
        if first is not second:
            message = msg or f"{first} is not {second}"
            raise AssertionError(message)

    def assertIsNot(self, first, second, msg=None):
        if first is second:
            message = msg or f"{first} is {second}"
            raise AssertionError(message)

    def assertIsNone(self, expr, msg=None):
        if expr is not None:
            message = msg or f"Expected None, got {expr}"
            raise AssertionError(message)

    def assertIsNotNone(self, expr, msg=None):
        if expr is None:
            message = msg or "Expected non-None value"
            raise AssertionError(message)

    def assertIn(self, member, container, msg=None):
        if member not in container:
            message = msg or f"{member} not in {container}"
            raise AssertionError(message)

    def assertNotIn(self, member, container, msg=None):
        if member in container:
            message = msg or f"{member} in {container}"
            raise AssertionError(message)

    # Context manager for assertRaises
    def assertRaises(self, exception):
        # Return a context manager
        return _AssertRaisesContext(exception, self)

class _AssertRaisesContext:
    def __init__(self, expected, test_case):
        self.expected = expected
        self.test_case = test_case

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is None:
            raise AssertionError(f"{self.expected} not raised")
        # Suppress the exception if it matches
        return True

# Simple test runner
def main():
    """Run all tests in the current module"""
    import sys

    # Get all classes from current module
    # This is simplified - real implementation would use introspection
    print("Running tests...")

    # For now, just a placeholder
    print("unittest.main() - test discovery not yet implemented")

# Make unittest importable
__all__ = ['TestCase', 'main']
```

**Files to create:**
- `modules/unittest.m28` or `builtin/modules/unittest.go`

### Phase 2: Test Discovery & Running (MVP Runner)

**Estimated effort: 4-8 hours**

Need to implement:

1. **Introspection capabilities** (if not already available):
   ```python
   # Need to get all classes in a module
   # Need to check if class is subclass of TestCase
   # Need to get all methods starting with 'test_'
   ```

2. **Simple test runner**:
   ```python
   class TestRunner:
       def discover_tests(self, module):
           """Find all TestCase subclasses and test methods"""
           tests = []
           for name in dir(module):
               obj = getattr(module, name)
               if is_class(obj) and issubclass(obj, TestCase):
                   for method_name in dir(obj):
                       if method_name.startswith('test_'):
                           tests.append((obj, method_name))
           return tests

       def run_test(self, test_class, method_name):
           """Run a single test method"""
           instance = test_class()
           try:
               instance.setUp()
               method = getattr(instance, method_name)
               method()
               instance.tearDown()
               return ('pass', None)
           except AssertionError as e:
               return ('fail', str(e))
           except Exception as e:
               return ('error', str(e))

       def run_all(self, tests):
           """Run all tests and report results"""
           passed = 0
           failed = 0
           errors = 0

           for test_class, method_name in tests:
               status, msg = self.run_test(test_class, method_name)
               if status == 'pass':
                   print('.', end='')
                   passed += 1
               elif status == 'fail':
                   print('F', end='')
                   failed += 1
               else:
                   print('E', end='')
                   errors += 1

           print()
           print(f"\n{passed} passed, {failed} failed, {errors} errors")
   ```

**Dependencies needed:**
- `dir()` builtin (check if exists)
- `getattr()` builtin (check if exists)
- `issubclass()` builtin (check if exists)
- `isinstance()` builtin (check if exists)

### Phase 3: Additional Assertions

**Estimated effort: 2-4 hours**

Implement remaining common assertions:

```python
def assertGreater(self, a, b, msg=None): ...
def assertLess(self, a, b, msg=None): ...
def assertGreaterEqual(self, a, b, msg=None): ...
def assertLessEqual(self, a, b, msg=None): ...
def assertAlmostEqual(self, first, second, places=7, msg=None): ...
def assertListEqual(self, list1, list2, msg=None): ...
def assertDictEqual(self, dict1, dict2, msg=None): ...
def assertSetEqual(self, set1, set2, msg=None): ...
def assertTupleEqual(self, tuple1, tuple2, msg=None): ...
def assertRaisesRegex(self, exception, regex): ...
```

### Phase 4: Advanced Features (Optional)

**Estimated effort: 8+ hours**

- Test suites and suite composition
- Class-level setUp/tearDown (setUpClass, tearDownClass)
- Module-level setUp/tearDown
- Test skipping (@skip, @skipIf, @skipUnless)
- Expected failures (@expectedFailure)
- Subtests (self.subTest())
- Test discovery from file system
- Verbose output modes
- XML/JSON test results

## Quick Implementation Plan

### Step 1: Check What M28 Already Has

```bash
# Check if these builtins exist
./bin/m28 -c 'print(dir())'
./bin/m28 -c 'print(getattr)'
./bin/m28 -c 'print(isinstance)'
./bin/m28 -c 'print(issubclass)'
```

### Step 2: Implement Basic TestCase

Create `modules/unittest.m28` with the Phase 1 code above.

### Step 3: Test It

```python
from unittest import TestCase

class MyTest(TestCase):
    def test_simple(self):
        self.assertEqual(1 + 1, 2)

    def test_list(self):
        self.assertIn(2, [1, 2, 3])

# For now, manually run tests
t = MyTest()
t.test_simple()
t.test_list()
print("Tests passed!")
```

### Step 4: Add Test Runner

Implement test discovery and running from Phase 2.

### Step 5: Test with Real CPython Tests

```bash
# Try running a real CPython test
./bin/m28 tests/cpython-source/Lib/test/test_bool.py
```

## Easier Alternative: Minimal Test Adapter

**Estimated effort: 30 minutes**

Instead of full unittest, create a simple adapter:

```python
# tests/unittest_adapter.m28
# Minimal adapter to make CPython tests runnable

class TestCase:
    def assertEqual(self, a, b, msg=None):
        assert a == b, msg or f"{a} != {b}"

    def assertTrue(self, x, msg=None):
        assert x, msg or f"Expected True"

    def assertFalse(self, x, msg=None):
        assert not x, msg or f"Expected False"

    def assertIn(self, a, b, msg=None):
        assert a in b, msg or f"{a} not in {b}"

    def assertIsNone(self, x, msg=None):
        assert x is None, msg or f"Expected None"

    def setUp(self):
        pass

    def tearDown(self):
        pass

def main():
    print("Tests would run here")
```

Then modify CPython tests to use this minimal adapter.

## Recommendation

**Start with the Minimal Test Adapter approach:**

1. ✅ Create `modules/unittest.m28` with basic TestCase
2. ✅ Implement 10 core assertions
3. ✅ Test with simple examples
4. ✅ Gradually add more assertions as needed
5. ⏳ Add test discovery later if valuable

**Total time for MVP: ~4-6 hours of focused work**

This gets you 80% of the value (running most CPython tests) with 20% of the effort (no test discovery/runner complexity yet).

## Want Me to Implement It?

I can implement:
1. ✅ Basic unittest.TestCase with 10 assertions (~30 min)
2. ✅ Test it with a few examples (~15 min)
3. ✅ Create a guide for using it (~15 min)

Then you can:
- Run simpler CPython tests manually
- Gradually add more assertions
- Add test discovery later if needed

Should I start with the basic implementation?
