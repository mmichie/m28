# Test string interning behavior with 'is' operator
# This test verifies that equal strings are treated as the same object
# which is critical for patterns like argparse.SUPPRESS

# Test 1: String literals should be interned
s1 = "hello"
s2 = "hello"
assert s1 is s2, "Equal string literals should be the same object"
print("✓ String literals are interned")

# Test 2: String constants from modules should be interned
import argparse
suppress1 = argparse.SUPPRESS
suppress2 = argparse.SUPPRESS
assert suppress1 is suppress2, "argparse.SUPPRESS should be the same object"
print("✓ Module constants are interned")

# Test 3: Equal strings should be identical
x = "==SUPPRESS=="
y = "==SUPPRESS=="
assert x is y, "Equal strings should be identical"
print("✓ Equal strings are identical")

# Test 4: Different strings are not identical
a = "foo"
b = "bar"
assert not (a is b), "Different strings should not be identical"
print("✓ Different strings are not identical")

# Test 5: String equality vs identity
c = "test"
d = "test"
assert c == d, "Equal strings should be equal"
assert c is d, "Equal strings should be identical (interned)"
print("✓ String equality and identity both work")

# Test 6: None identity
assert None is None, "None should be identical to None"
print("✓ None identity works")

# Test 7: Boolean identity
assert True is True, "True should be identical to True"
assert False is False, "False should be identical to False"
assert not (True is False), "True and False should not be identical"
print("✓ Boolean identity works")

# Test 8: List identity (different objects even if equal)
list1 = [1, 2, 3]
list2 = [1, 2, 3]
assert list1 == list2, "Equal lists should be equal"
assert not (list1 is list2), "Different list objects should not be identical"
print("✓ List identity works correctly")

# Test 9: Same list object
list3 = [4, 5, 6]
list4 = list3
assert list3 is list4, "Same list object should be identical"
print("✓ Same object identity works")

print("\nAll string interning tests passed!")
