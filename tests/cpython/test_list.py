"""
Test list operations and methods
Adapted from CPython's Lib/test/test_list.py for M28
"""

print("=== List Tests ===")

# ============================================================================
# List Creation
# ============================================================================
print("\n1. Testing list creation...")

# Empty list
lst = []
assert len(lst) == 0

# List with elements
lst = [1, 2, 3]
assert len(lst) == 3
assert lst == [1, 2, 3]

# Mixed types
lst = [1, "two", 3.0, True, None]
assert len(lst) == 5

print("✓ List creation passed")

# ============================================================================
# Indexing
# ============================================================================
print("\n2. Testing indexing...")

lst = [10, 20, 30, 40, 50]

# Positive indexing
assert lst[0] == 10
assert lst[1] == 20
assert lst[4] == 50

# Negative indexing
assert lst[-1] == 50
assert lst[-2] == 40
assert lst[-5] == 10

print("✓ Indexing passed")

# ============================================================================
# Slicing
# ============================================================================
print("\n3. Testing slicing...")

lst = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

# Basic slicing
assert lst[2:5] == [2, 3, 4]
assert lst[:3] == [0, 1, 2]
assert lst[7:] == [7, 8, 9]

# Step slicing
assert lst[::2] == [0, 2, 4, 6, 8]
assert lst[1::2] == [1, 3, 5, 7, 9]

# Negative step
assert lst[::-1] == [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

print("✓ Slicing passed")

# ============================================================================
# List Methods - append, extend, insert
# ============================================================================
print("\n4. Testing append, extend, insert...")

# append
lst = [1, 2, 3]
lst.append(4)
assert lst == [1, 2, 3, 4]

# extend
lst = [1, 2, 3]
lst.extend([4, 5, 6])
assert lst == [1, 2, 3, 4, 5, 6]

# insert
lst = [1, 2, 4]
lst.insert(2, 3)
assert lst == [1, 2, 3, 4]

print("✓ append, extend, insert passed")

# ============================================================================
# List Methods - remove, pop, clear
# ============================================================================
print("\n5. Testing remove, pop, clear...")

# remove
lst = [1, 2, 3, 2, 4]
lst.remove(2)  # Removes first occurrence
assert lst == [1, 3, 2, 4]

# pop
lst = [1, 2, 3, 4]
x = lst.pop()
assert x == 4
assert lst == [1, 2, 3]

x = lst.pop(0)
assert x == 1
assert lst == [2, 3]

# clear
lst = [1, 2, 3]
lst.clear()
assert lst == []

print("✓ remove, pop, clear passed")

# ============================================================================
# List Methods - index, count
# ============================================================================
print("\n6. Testing index, count...")

lst = [1, 2, 3, 2, 4, 2]

# index
assert lst.index(2) == 1  # First occurrence
assert lst.index(4) == 4

# count
assert lst.count(2) == 3
assert lst.count(1) == 1
assert lst.count(99) == 0

print("✓ index, count passed")

# ============================================================================
# List Methods - sort, reverse
# ============================================================================
print("\n7. Testing sort, reverse...")

# sort
lst = [3, 1, 4, 1, 5, 9, 2, 6]
lst.sort()
assert lst == [1, 1, 2, 3, 4, 5, 6, 9]

# sort reverse
lst = [3, 1, 4, 1, 5]
lst.sort(reverse=True)
assert lst == [5, 4, 3, 1, 1]

# reverse
lst = [1, 2, 3, 4, 5]
lst.reverse()
assert lst == [5, 4, 3, 2, 1]

print("✓ sort, reverse passed")

# ============================================================================
# List Operators
# ============================================================================
print("\n8. Testing list operators...")

# Concatenation
lst1 = [1, 2, 3]
lst2 = [4, 5, 6]
result = lst1 + lst2
assert result == [1, 2, 3, 4, 5, 6]
assert lst1 == [1, 2, 3]  # Original unchanged

# Repetition
lst = [1, 2]
result = lst * 3
assert result == [1, 2, 1, 2, 1, 2]

# Membership
lst = [1, 2, 3, 4, 5]
assert 3 in lst
assert 6 not in lst

print("✓ List operators passed")

# ============================================================================
# List Assignment
# ============================================================================
print("\n9. Testing list assignment...")

# Item assignment
lst = [1, 2, 3, 4, 5]
lst[0] = 10
assert lst == [10, 2, 3, 4, 5]

lst[-1] = 50
assert lst == [10, 2, 3, 4, 50]

# Slice assignment
lst = [1, 2, 3, 4, 5]
lst[1:3] = [20, 30]
assert lst == [1, 20, 30, 4, 5]

print("✓ List assignment passed")

# ============================================================================
# List Comprehensions
# ============================================================================
print("\n10. Testing list comprehensions...")

# Basic comprehension
squares = [x * x for x in range(5)]
assert squares == [0, 1, 4, 9, 16]

# With condition
evens = [x for x in range(10) if x % 2 == 0]
assert evens == [0, 2, 4, 6, 8]

# With expression
doubled = [x * 2 for x in [1, 2, 3]]
assert doubled == [2, 4, 6]

# Nested
matrix = [[1, 2], [3, 4], [5, 6]]
flat = [item for row in matrix for item in row]
assert flat == [1, 2, 3, 4, 5, 6]

print("✓ List comprehensions passed")

# ============================================================================
# Edge Cases
# ============================================================================
print("\n11. Testing edge cases...")

# Empty list operations
lst = []
assert len(lst) == 0
assert list(reversed(lst)) == []

# Single element
lst = [42]
assert len(lst) == 1
assert lst[0] == 42
assert lst[-1] == 42

# Copying
lst1 = [1, 2, 3]
lst2 = lst1.copy()
lst2.append(4)
assert lst1 == [1, 2, 3]
assert lst2 == [1, 2, 3, 4]

print("✓ Edge cases passed")

# ============================================================================
# Summary
# ============================================================================
print("\n" + "="*60)
print("✅ All List Tests Passed!")
print("="*60)
print("\nFeatures tested:")
print("  ✓ List creation and initialization")
print("  ✓ Indexing (positive and negative)")
print("  ✓ Slicing (with steps)")
print("  ✓ append, extend, insert methods")
print("  ✓ remove, pop, clear methods")
print("  ✓ index, count methods")
print("  ✓ sort, reverse methods")
print("  ✓ Concatenation and repetition")
print("  ✓ List assignment")
print("  ✓ List comprehensions")
print("  ✓ Edge cases")
