# Test generator expressions
# Python syntax: (expr for item in iterable if condition)

print("Test 1: Basic generator expression")
gen = (x * 2 for x in range(5))
result = list(gen)
print(f"Result: {result}")
assert result == [0, 2, 4, 6, 8], f"Expected [0, 2, 4, 6, 8], got {result}"
print("✓ Test 1 passed")

print("\nTest 2: Generator expression with filter")
gen = (x for x in range(10) if x % 2 == 0)
result = list(gen)
print(f"Result: {result}")
assert result == [0, 2, 4, 6, 8], f"Expected [0, 2, 4, 6, 8], got {result}"
print("✓ Test 2 passed")

print("\nTest 3: Generator expression with nested loop")
gen = (x * y for x in range(3) for y in range(3))
result = list(gen)
print(f"Result: {result}")
assert result == [0, 0, 0, 0, 1, 2, 0, 2, 4], f"Expected [0, 0, 0, 0, 1, 2, 0, 2, 4], got {result}"
print("✓ Test 3 passed")

print("\nTest 4: Generator expression laziness")
def side_effect():
    print("  (generator evaluated)")
    return [1, 2, 3]

gen = (x * 2 for x in side_effect())
print("  Generator created (should not print 'generator evaluated' yet)")
result = list(gen)
print(f"Result: {result}")
assert result == [2, 4, 6], f"Expected [2, 4, 6], got {result}"
print("✓ Test 4 passed")

print("\n✅ All generator expression tests passed!")
