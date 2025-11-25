# Test lazy generator evaluation with infinite iterables
# This tests the key feature: generators don't consume the source until Next() is called

print("Testing lazy generator evaluation...")

# Test 1: Creating generator from infinite iterable doesn't hang
print("\n1. Creating generator from infinite iterable:")


def count(start=0):
    while True:
        yield start
        start += 1


# This should return immediately (not hang)
g = (x * 2 for x in count())
print("   ✓ Generator created from infinite iterable (didn't hang)")

# Consume a few values
assert next(g) == 0, "First value should be 0"
assert next(g) == 2, "Second value should be 2"
assert next(g) == 4, "Third value should be 4"
print("   ✓ First few values: 0, 2, 4")

# Test 2: Nested generator with infinite outer loop and condition
print("\n2. Nested generator with infinite loop and condition:")


def count_from_five():
    x = 5
    while True:
        yield x
        x += 1


g = (x * y for x in count_from_five() for y in range(3) if x > 5 and x < 8)
# Should get: x=6 with y=0,1,2 then x=7 with y=0,1,2
results = []
for i in range(6):
    results.append(next(g))

expected = [0, 6, 12, 0, 7, 14]  # 6*0, 6*1, 6*2, 7*0, 7*1, 7*2
assert results == expected, f"Expected {expected}, got {results}"
print(f"   ✓ Results: {results}")

# Test 3: Generator with large range (memory efficient)
print("\n3. Memory-efficient processing of large range:")
# Old eager implementation would consume all 1 million items upfront
# Lazy implementation only processes what we need
g = (x * 2 for x in range(1000000))
# Take just the first 5
results = [next(g) for _ in range(5)]
expected = [0, 2, 4, 6, 8]
assert results == expected, f"Expected {expected}, got {results}"
print(f"   ✓ First 5 from large range: {results}")

# Test 4: Generator chain doesn't evaluate until consumed
print("\n4. Generator chains are lazy:")
g1 = (x for x in range(100))
g2 = (x * 2 for x in g1)
g3 = (x + 1 for x in g2)
# All generators created instantly - no evaluation yet
first = next(g3)
assert first == 1, f"Expected 1, got {first}"  # (0 * 2) + 1 = 1
second = next(g3)
assert second == 3, f"Expected 3, got {second}"  # (1 * 2) + 1 = 3
print(f"   ✓ Generator chain works: first={first}, second={second}")

# Test 5: Infinite nested generator with manual consumption
print("\n5. Infinite nested generator with condition:")


def infinite_range():
    i = 0
    while True:
        yield i
        i += 1


# Nested infinite generators - manually consume values we know will pass condition
g = (x + y for x in infinite_range() for y in range(2) if x < 3)
results = []
# We know x=0,1,2 will pass the condition (x < 3), giving us 6 values
for _ in range(6):
    results.append(next(g))

expected = [0, 1, 1, 2, 2, 3]  # x=0: 0,1; x=1: 1,2; x=2: 2,3
assert results == expected, f"Expected {expected}, got {results}"
print(f"   ✓ Infinite nested with condition: {results}")

# Test 6: Variable-dependent iterable with finite consumption
print("\n6. Variable-dependent iterable with condition:")


def make_range(n):
    """Helper to create range(n)"""
    return range(n)


g = (x * y for x in infinite_range() for y in make_range(x) if x < 4 and x > 0)
results = []
# Manually consume values we know will pass: x=1,2,3 (6 values total)
for _ in range(6):
    results.append(next(g))

# x=1: y=0 → 0
# x=2: y=0,1 → 0,2
# x=3: y=0,1,2 → 0,3,6
expected = [0, 0, 2, 0, 3, 6]
assert results == expected, f"Expected {expected}, got {results}"
print(f"   ✓ Variable-dependent with condition: {results}")

print("\n✅ All lazy evaluation tests passed!")
print("   Lazy generators successfully handle:")
print("   - Infinite iterables")
print("   - Large ranges without memory overhead")
print("   - Generator chains")
print("   - Nested infinite generators with conditions")
