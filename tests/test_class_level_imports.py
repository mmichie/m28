#!/usr/bin/env python3
# Test class-level import statements
# This feature allows imports inside class definitions,
# making imported names available as class attributes

print("Testing class-level import statements...")

# Test 1: Simple import with single name
class TestSimpleImport:
    from sys import version

obj1 = TestSimpleImport()
assert hasattr(obj1, 'version'), "TestSimpleImport should have version attribute"
print("✓ Test 1: Simple import with single name passed")

# Test 2: Import multiple names
class TestMultipleImports:
    from sys import version
    from os import path

obj2 = TestMultipleImports()
assert hasattr(obj2, 'version'), "TestMultipleImports should have version attribute"
assert hasattr(obj2, 'path'), "TestMultipleImports should have path attribute"
print("✓ Test 2: Import multiple names passed")

# Test 3: Use imported name in class method
class TestImportInMethod:
    from sys import version

    def get_version(self):
        return self.version

obj3 = TestImportInMethod()
result = obj3.get_version()
assert result, "get_version should return the version"
print("✓ Test 3: Use imported name in class method passed")

# Test 4: Access as class attribute (not just instance attribute)
class TestClassAttribute:
    from sys import version

assert hasattr(TestClassAttribute, 'version'), "Should be accessible as class attribute"
print("✓ Test 4: Access as class attribute passed")

# Test 5: CPython-style unittest pattern
import unittest

class TestUnittestPattern(unittest.TestCase):
    from test.support.warnings_helper import check_syntax_warning

    def test_has_import(self):
        assert hasattr(self, 'check_syntax_warning'), "Should have check_syntax_warning"
        return True

obj5 = TestUnittestPattern()
obj5.test_has_import()
print("✓ Test 5: CPython-style unittest pattern passed")

print("\n✅ All class-level import tests passed!")
