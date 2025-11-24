# Test tuple unpacking in for loops
# This is a common pattern that should work in Python

print("Test 1: Basic tuple unpacking in for loop")
pairs = [(1, 2), (3, 4), (5, 6)]
result = []
for a, b in pairs:
    result.append(a + b)
print(f"Result: {result}")
assert result == [3, 7, 11], f"Expected [3, 7, 11], got {result}"
print("✓ Test 1 passed")

print("\nTest 2: Triple unpacking")
triples = [(1, 2, 3), (4, 5, 6)]
result = []
for a, b, c in triples:
    result.append(a + b + c)
print(f"Result: {result}")
assert result == [6, 15], f"Expected [6, 15], got {result}"
print("✓ Test 2 passed")

print("\nTest 3: Nested unpacking")
nested = [((1, 2), 3), ((4, 5), 6)]
result = []
for (a, b), c in nested:
    result.append(a + b + c)
print(f"Result: {result}")
assert result == [6, 15], f"Expected [6, 15], got {result}"
print("✓ Test 3 passed")

print("\nTest 4: Dict items unpacking")
d = {"a": 1, "b": 2, "c": 3}
result = []
for key, value in d.items():
    result.append(f"{key}={value}")
result.sort()
print(f"Result: {result}")
assert result == ["a=1", "b=2", "c=3"], f"Expected ['a=1', 'b=2', 'c=3'], got {result}"
print("✓ Test 4 passed")

print("\n✅ All tuple unpacking tests passed!")
