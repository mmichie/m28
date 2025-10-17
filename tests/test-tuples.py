# Test tuple literals

print("Testing tuple literals...")

# Test 1: Empty tuple
empty = ()
print(f"Empty tuple: {empty}, length: {len(empty)}")

# Test 2: Single element tuple (with trailing comma)
single = (42,)
print(f"Single element: {single}, length: {len(single)}")

# Test 3: Two element tuple
pair = (1, 2)
print(f"Pair: {pair}, length: {len(pair)}")

# Test 4: Multi-element tuple
triple = (10, 20, 30)
print(f"Triple: {triple}, length: {len(triple)}")

# Test 5: Tuple with expressions
x = 5
y = 10
calc = (x, y, x + y)
print(f"Calculated: {calc}")

# Test 6: Nested tuples
nested = ((1, 2), (3, 4))
print(f"Nested: {nested}")

# Test 7: Parenthesized expression (not a tuple)
not_a_tuple = (42)
print(f"Not a tuple (just 42): {not_a_tuple}")

# Test 8: Tuple indexing
coords = (3, 4, 5)
print(f"coords[0] = {coords[0]}, coords[1] = {coords[1]}, coords[2] = {coords[2]}")

print("✓ Tuple tests complete")
