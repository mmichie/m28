# Test basic nested generator expression functionality

print("Testing nested generator expressions...")

# Double nested
print("\n1. Double nested generator:")
g = (x*y for x in range(3) for y in range(3))
result = list(g)
expected = [0, 0, 0, 0, 1, 2, 0, 2, 4]
assert result == expected, f"Double nested failed: got {result}, expected {expected}"
print(f"   ✓ (x*y for x in range(3) for y in range(3)) = {result}")

# Triple nested
print("\n2. Triple nested generator:")
g = (x+y+z for x in [1,2] for y in [10,20] for z in [100,200])
result = list(g)
expected = [111, 211, 121, 221, 112, 212, 122, 222]
assert result == expected, f"Triple nested failed: got {result}, expected {expected}"
print(f"   ✓ (x+y+z for x in [1,2] for y in [10,20] for z in [100,200]) = {result}")

# With condition
print("\n3. Generator with conditions:")
g = (x*y for x in range(5) for y in range(5) if x>2)
result = list(g)
expected = [0, 3, 6, 9, 12, 0, 4, 8, 12, 16]
assert result == expected, f"With condition failed: got {result}, expected {expected}"
print(f"   ✓ (x*y for x in range(5) for y in range(5) if x>2) = {result}")

# Multiple conditions
print("\n4. Generator with multiple conditions:")
g = (x*y for x in range(5) if x%2==0 for y in range(5) if y>2)
result = list(g)
expected = [0, 0, 6, 8, 12, 16]
assert result == expected, f"Multiple conditions failed: got {result}, expected {expected}"
print(f"   ✓ (x*y for x in range(5) if x%2==0 for y in range(5) if y>2) = {result}")

# Compare with list comprehension
print("\n5. Generator vs list comprehension:")
gen_result = list((x*y for x in range(3) for y in range(3)))
list_result = [x*y for x in range(3) for y in range(3)]
assert gen_result == list_result, f"Gen/list mismatch: gen={gen_result}, list={list_result}"
print(f"   ✓ Generator result matches list comprehension: {gen_result}")

# Variable dependency (inner loop depends on outer variable)
print("\n6. Variable dependency:")
g = (x*y for x in range(4) for y in range(x))
result = list(g)
expected = [0, 0, 2, 0, 3, 6]
assert result == expected, f"Variable dependency failed: got {result}, expected {expected}"
print(f"   ✓ (x*y for x in range(4) for y in range(x)) = {result}")

print("\n✅ All basic nested generator tests passed!")
