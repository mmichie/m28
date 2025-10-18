"""
Test built-in functions
Adapted from CPython's Lib/test/test_builtin.py for M28
"""

print("=== Built-in Functions Tests ===")

# ============================================================================
# Type Conversion Functions
# ============================================================================
print("\n1. Testing type conversions...")

# int()
assert int("42") == 42
assert int(3.14) == 3
assert int(True) == 1
assert int(False) == 0

# float()
assert float("3.14") == 3.14
assert float(42) == 42.0
assert float(True) == 1.0

# str()
assert str(42) == "42"
assert str(3.14) == "3.14"
assert str(True) == "True"
assert str([1, 2, 3]) == "[1, 2, 3]"

# bool()
assert bool(1) is True
assert bool(0) is False
assert bool("") is False
assert bool("hello") is True
assert bool([]) is False
assert bool([1]) is True

print("✓ Type conversions passed")

# ============================================================================
# Container Functions
# ============================================================================
print("\n2. Testing container functions...")

# len()
assert len([1, 2, 3]) == 3
assert len("hello") == 5
assert len({"a": 1, "b": 2}) == 2
assert len({1, 2, 3}) == 3
assert len((1, 2)) == 2

# max()
assert max([1, 5, 3, 2, 4]) == 5
assert max(1, 5, 3, 2, 4) == 5
assert max("hello") == "o"

# min()
assert min([1, 5, 3, 2, 4]) == 1
assert min(1, 5, 3, 2, 4) == 1
assert min("hello") == "e"

# sum()
assert sum([1, 2, 3, 4, 5]) == 15
assert sum([1, 2, 3], 10) == 16  # With start value
assert sum([]) == 0

# sorted()
assert sorted([3, 1, 4, 1, 5]) == [1, 1, 3, 4, 5]
assert sorted("hello") == ["e", "h", "l", "l", "o"]
assert sorted([3, 1, 2], reverse=True) == [3, 2, 1]

# reversed()
assert list(reversed([1, 2, 3])) == [3, 2, 1]
assert list(reversed("abc")) == ["c", "b", "a"]

print("✓ Container functions passed")

# ============================================================================
# Iterator Functions
# ============================================================================
print("\n3. Testing iterator functions...")

# range()
assert list(range(5)) == [0, 1, 2, 3, 4]
assert list(range(2, 7)) == [2, 3, 4, 5, 6]
assert list(range(0, 10, 2)) == [0, 2, 4, 6, 8]

# enumerate()
items = ["a", "b", "c"]
result = list(enumerate(items))
assert result == [(0, "a"), (1, "b"), (2, "c")]

# enumerate with start
result = list(enumerate(items, start=1))
assert result == [(1, "a"), (2, "b"), (3, "c")]

# zip()
a = [1, 2, 3]
b = ["a", "b", "c"]
result = list(zip(a, b))
assert result == [(1, "a"), (2, "b"), (3, "c")]

# zip with different lengths
a = [1, 2, 3, 4]
b = ["a", "b"]
result = list(zip(a, b))
assert result == [(1, "a"), (2, "b")]

# map()
nums = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x * x, nums))
assert squared == [1, 4, 9, 16, 25]

# filter()
nums = [1, 2, 3, 4, 5, 6]
evens = list(filter(lambda x: x % 2 == 0, nums))
assert evens == [2, 4, 6]

print("✓ Iterator functions passed")

# ============================================================================
# Type Checking Functions
# ============================================================================
print("\n4. Testing type checking...")

# isinstance()
assert isinstance(42, int)
assert isinstance("hello", str)
assert isinstance([1, 2], list)
assert isinstance({"a": 1}, dict)
assert not isinstance(42, str)

# type()
assert type(42) == int
assert type("hello") == str
assert type([1, 2]) == list
assert type({"a": 1}) == dict

print("✓ Type checking passed")

# ============================================================================
# Any/All
# ============================================================================
print("\n5. Testing any/all...")

# any()
assert any([True, False, False]) is True
assert any([False, False, False]) is False
assert any([]) is False
assert any([0, 1, 2]) is True  # 1 and 2 are truthy

# all()
assert all([True, True, True]) is True
assert all([True, False, True]) is False
assert all([]) is True  # Empty is True!
assert all([1, 2, 3]) is True  # All truthy
assert all([1, 0, 3]) is False  # 0 is falsy

print("✓ any/all passed")

# ============================================================================
# abs, round, pow
# ============================================================================
print("\n6. Testing math functions...")

# abs()
assert abs(42) == 42
assert abs(-42) == 42
assert abs(0) == 0
assert abs(-3.14) == 3.14

# round()
assert round(3.14159) == 3
assert round(3.14159, 2) == 3.14
assert round(2.5) == 2  # Banker's rounding
assert round(3.5) == 4

# pow()
assert pow(2, 3) == 8
assert pow(2, 8) == 256
assert pow(5, 2) == 25

print("✓ Math functions passed")

# ============================================================================
# chr, ord
# ============================================================================
print("\n7. Testing chr/ord...")

# chr()
assert chr(65) == "A"
assert chr(97) == "a"
assert chr(48) == "0"

# ord()
assert ord("A") == 65
assert ord("a") == 97
assert ord("0") == 48

# Round trip
assert chr(ord("X")) == "X"
assert ord(chr(100)) == 100

print("✓ chr/ord passed")

# ============================================================================
# divmod
# ============================================================================
print("\n8. Testing divmod...")

# divmod()
assert divmod(10, 3) == (3, 1)
assert divmod(17, 5) == (3, 2)
assert divmod(100, 7) == (14, 2)

# Relationship: divmod(a, b) == (a // b, a % b)
a, b = 37, 5
q, r = divmod(a, b)
assert q == a // b
assert r == a % b

print("✓ divmod passed")

# ============================================================================
# repr
# ============================================================================
print("\n9. Testing repr...")

# repr()
assert repr(42) == "42"
assert repr("hello") == "'hello'"
assert repr([1, 2, 3]) == "[1, 2, 3]"
assert repr({"a": 1}) in ["{'a': 1}", "{'a':1}"]  # Spacing may vary

print("✓ repr passed")

# ============================================================================
# input/print (basic)
# ============================================================================
print("\n10. Testing print...")

# print() - just ensure it doesn't error
print("test")
print("multiple", "arguments")
print("with", "sep", sep="-")
print("end test", end="!\n")

print("✓ print passed")

# ============================================================================
# Summary
# ============================================================================
print("\n" + "="*60)
print("✅ All Built-in Function Tests Passed!")
print("="*60)
print("\nFunctions tested:")
print("  ✓ Type conversions (int, float, str, bool)")
print("  ✓ Container functions (len, max, min, sum, sorted)")
print("  ✓ Iterator functions (range, enumerate, zip, map, filter)")
print("  ✓ Type checking (isinstance, type)")
print("  ✓ any/all")
print("  ✓ Math functions (abs, round, pow)")
print("  ✓ chr/ord")
print("  ✓ divmod")
print("  ✓ repr")
print("  ✓ print")
