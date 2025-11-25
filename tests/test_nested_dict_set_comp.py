# Test nested dict and set comprehensions
# Verifies M28-8154: Add dict and set comprehension multi-clause support

print("Testing nested dict and set comprehensions...")

# Test case 1: Basic nested set comprehension (from issue)
result = {x*y for x in range(3) for y in range(3)}
expected = {0, 1, 2, 4}  # 0*0=0, 1*1=1, 1*2=2, 2*2=4 (unique values)
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Basic nested set: {result}")

# Test case 2: Basic nested dict comprehension (from issue)
result = {x:y for x in range(3) for y in range(3)}
# Note: dict keys are unique, so x=0,1,2 each get overwritten with last y value
expected = {0: 2, 1: 2, 2: 2}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Basic nested dict: {result}")

# Test case 3: Nested set with multiple ranges
result = {x + y for x in range(2) for y in range(3)}
expected = {0, 1, 2, 3}  # 0+0, 0+1, 0+2, 1+0, 1+1, 1+2
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Set with addition: {result}")

# Test case 4: Nested dict with computed keys and values
result = {x+y: x*y for x in range(1, 3) for y in range(1, 3)}
expected = {2: 1, 3: 2, 4: 4}  # (1+1):1*1, (1+2):1*2, (2+1):2*1, (2+2):2*2
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Dict with computed keys/values: {result}")

# Test case 5: Triple nested set comprehension
result = {x+y+z for x in [1, 2] for y in [10, 20] for z in [100, 200]}
expected = {111, 211, 121, 221, 112, 212, 122, 222}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Triple nested set: {result}")

# Test case 6: Nested set with condition
result = {x*y for x in range(4) for y in range(4) if x > 0 and y > 0}
expected = {1, 2, 3, 4, 6, 9}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Nested set with condition: {result}")

# Test case 7: Nested dict with condition
result = {x: y for x in range(3) for y in range(5) if y % 2 == 0}
expected = {0: 4, 1: 4, 2: 4}  # Last even y is 4
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Nested dict with condition: {result}")

# Test case 8: Set comprehension with list iteration
matrix = [[1, 2], [3, 4], [5, 6]]
result = {item * 2 for row in matrix for item in row}
expected = {2, 4, 6, 8, 10, 12}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Set with matrix: {result}")

# Test case 9: Dict comprehension with list iteration
data = [("a", 1), ("b", 2), ("c", 3)]
result = {k+str(i): v*i for i in [1, 2] for k, v in data}
expected = {"a1": 1, "b1": 2, "c1": 3, "a2": 2, "b2": 4, "c2": 6}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Dict with tuple unpacking: {result}")

# Test case 10: Empty result cases
result = {x for x in range(3) for y in range(0)}
expected = set()
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Empty set from empty inner range: {result}")

result = {x:y for x in range(0) for y in range(3)}
expected = {}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Empty dict from empty outer range: {result}")

# Test case 11: Nested set with string concatenation
result = {x + y for x in ["a", "b"] for y in ["1", "2"]}
expected = {"a1", "a2", "b1", "b2"}
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Set with strings: {result}")

# Test case 12: Dict with multiple conditions
result = {x: y for x in range(5) for y in range(5) if x < 3 and y > 2}
expected = {0: 4, 1: 4, 2: 4}  # x in [0,1,2], y in [3,4], last y wins
assert result == expected, f"Expected {expected}, got {result}"
print(f"✓ Dict with multiple conditions: {result}")

print("\n✓ All nested dict/set comprehension tests passed!")
