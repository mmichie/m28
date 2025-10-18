"""
Test dictionary operations and methods
Adapted from CPython's Lib/test/test_dict.py for M28
"""

print("=== Dictionary Tests ===")

# ============================================================================
# Dict Creation
# ============================================================================
print("\n1. Testing dict creation...")

# Empty dict
d = {}
assert len(d) == 0

# Dict with elements
d = {"a": 1, "b": 2, "c": 3}
assert len(d) == 3

# Mixed key/value types
d = {1: "one", "two": 2, 3.0: [3], (4,): {"four": 4}}
assert len(d) == 4

# Using dict() constructor
d = dict(a=1, b=2, c=3)
assert d == {"a": 1, "b": 2, "c": 3}

print("✓ Dict creation passed")

# ============================================================================
# Dict Access
# ============================================================================
print("\n2. Testing dict access...")

d = {"a": 1, "b": 2, "c": 3}

# Item access
assert d["a"] == 1
assert d["b"] == 2
assert d["c"] == 3

# get method
assert d.get("a") == 1
assert d.get("missing") is None
assert d.get("missing", "default") == "default"

print("✓ Dict access passed")

# ============================================================================
# Dict Modification
# ============================================================================
print("\n3. Testing dict modification...")

# Setting items
d = {}
d["a"] = 1
d["b"] = 2
assert d == {"a": 1, "b": 2}

# Updating values
d["a"] = 10
assert d["a"] == 10

# update method
d = {"a": 1, "b": 2}
d.update({"b": 20, "c": 30})
assert d == {"a": 1, "b": 20, "c": 30}

print("✓ Dict modification passed")

# ============================================================================
# Dict Methods - keys, values, items
# ============================================================================
print("\n4. Testing keys, values, items...")

d = {"a": 1, "b": 2, "c": 3}

# keys
keys = list(d.keys())
assert set(keys) == {"a", "b", "c"}

# values
values = list(d.values())
assert set(values) == {1, 2, 3}

# items
items = list(d.items())
assert set(items) == {("a", 1), ("b", 2), ("c", 3)}

print("✓ keys, values, items passed")

# ============================================================================
# Dict Methods - pop, popitem, clear
# ============================================================================
print("\n5. Testing pop, popitem, clear...")

# pop
d = {"a": 1, "b": 2, "c": 3}
val = d.pop("a")
assert val == 1
assert d == {"b": 2, "c": 3}

# pop with default
val = d.pop("missing", "default")
assert val == "default"

# popitem
d = {"a": 1}
key, val = d.popitem()
assert key == "a"
assert val == 1
assert len(d) == 0

# clear
d = {"a": 1, "b": 2}
d.clear()
assert d == {}

print("✓ pop, popitem, clear passed")

# ============================================================================
# Dict Methods - setdefault
# ============================================================================
print("\n6. Testing setdefault...")

d = {"a": 1}

# Key exists
val = d.setdefault("a", 99)
assert val == 1
assert d == {"a": 1}

# Key doesn't exist
val = d.setdefault("b", 2)
assert val == 2
assert d == {"a": 1, "b": 2}

print("✓ setdefault passed")

# ============================================================================
# Dict Operators
# ============================================================================
print("\n7. Testing dict operators...")

d = {"a": 1, "b": 2, "c": 3}

# Membership (keys)
assert "a" in d
assert "missing" not in d

# Length
assert len(d) == 3

# Equality
d1 = {"a": 1, "b": 2}
d2 = {"b": 2, "a": 1}  # Order doesn't matter
assert d1 == d2

d3 = {"a": 1, "b": 3}
assert d1 != d3

print("✓ Dict operators passed")

# ============================================================================
# Dict Comprehensions
# ============================================================================
print("\n8. Testing dict comprehensions...")

# Basic comprehension
squares = {x: x * x for x in range(5)}
assert squares == {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}

# With condition
evens = {x: x * 2 for x in range(10) if x % 2 == 0}
assert evens == {0: 0, 2: 4, 4: 8, 6: 12, 8: 16}

# From pairs
pairs = [("a", 1), ("b", 2), ("c", 3)]
d = {k: v for k, v in pairs}
assert d == {"a": 1, "b": 2, "c": 3}

print("✓ Dict comprehensions passed")

# ============================================================================
# Dict Iteration
# ============================================================================
print("\n9. Testing dict iteration...")

d = {"a": 1, "b": 2, "c": 3}

# Iterate over keys
keys = []
for k in d:
    keys.append(k)
assert set(keys) == {"a", "b", "c"}

# Iterate over items
items = []
for k, v in d.items():
    items.append((k, v))
assert set(items) == {("a", 1), ("b", 2), ("c", 3)}

print("✓ Dict iteration passed")

# ============================================================================
# Nested Dicts
# ============================================================================
print("\n10. Testing nested dicts...")

# Nested structure
d = {
    "user": {
        "name": "Alice",
        "age": 30,
        "address": {
            "city": "NYC",
            "zip": "10001"
        }
    }
}

assert d["user"]["name"] == "Alice"
assert d["user"]["address"]["city"] == "NYC"

print("✓ Nested dicts passed")

# ============================================================================
# Edge Cases
# ============================================================================
print("\n11. Testing edge cases...")

# Empty dict operations
d = {}
assert len(d) == 0
assert list(d.keys()) == []
assert list(d.values()) == []

# Single item
d = {"only": "one"}
assert len(d) == 1
assert d["only"] == "one"

# Hashable keys only (strings, numbers, tuples)
d = {
    "string": 1,
    42: 2,
    (1, 2): 3,
    True: 4,  # Note: True == 1 in Python
}
# Note: True and 1 are the same key
assert len(d) == 3  # "string", 42, (1,2) - True overwrites 42

# Copy
d1 = {"a": 1, "b": 2}
d2 = d1.copy()
d2["c"] = 3
assert d1 == {"a": 1, "b": 2}
assert d2 == {"a": 1, "b": 2, "c": 3}

print("✓ Edge cases passed")

# ============================================================================
# Summary
# ============================================================================
print("\n" + "="*60)
print("✅ All Dictionary Tests Passed!")
print("="*60)
print("\nFeatures tested:")
print("  ✓ Dict creation and initialization")
print("  ✓ Item access and get method")
print("  ✓ Modification and update")
print("  ✓ keys, values, items methods")
print("  ✓ pop, popitem, clear methods")
print("  ✓ setdefault method")
print("  ✓ Membership and operators")
print("  ✓ Dict comprehensions")
print("  ✓ Iteration")
print("  ✓ Nested dictionaries")
print("  ✓ Edge cases")
