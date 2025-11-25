# Comprehensive nested generator expression test suite
# Tests edge cases and patterns for nested generator expressions

# Test 1: Multiple conditions at different loop levels
print("Test 1: Multiple conditions at different loop levels")
g = (x*y for x in range(5) if x%2==0 for y in range(5) if y>2)
result = list(g)
expected = [0, 0, 6, 8, 12, 16]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS: {result}")

# Test 1b: Condition after each for clause
print("Test 1b: Condition after each for clause")
g = (x+y for x in range(10) if x>5 for y in range(10) if y<3)
result = list(g)
expected = [6, 7, 8, 7, 8, 9, 8, 9, 10, 9, 10, 11]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS: {result}")

# Test 1c: Multiple conditions on outer loop only
print("Test 1c: Multiple conditions on outer loop only")
g = (x*y for x in range(10) if x>2 if x<5 for y in range(3))
result = list(g)
expected = [0, 3, 6, 0, 4, 8]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS: {result}")

# Test 2: Variable dependencies - inner iterable depends on outer variable
print("\nTest 2: Variable dependencies")
g = (x*y for x in range(5) for y in range(x))
result = list(g)
expected = [0, 0, 2, 0, 3, 6, 0, 4, 8, 12]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS: {result}")

# Test 2b: More complex dependency
print("Test 2b: More complex dependency")
g = (x+y for x in range(4) for y in range(x, x+3))
result = list(g)
expected = [0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 8]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS: {result}")

# Test 3: Scope isolation - comprehension variables don't leak
print("\nTest 3: Scope isolation")
x = 100
y = 200
g = list((x*y for x in range(3) for y in range(2)))
assert x == 100, f"x leaked: {x}"
assert y == 200, f"y leaked: {y}"
print(f"  PASS: x={x}, y={y} (unchanged)")

# Test 3b: Inner variable shadowing outer
print("Test 3b: Inner variable shadowing outer")
x = 100
result = list((x for x in range(3)))
assert x == 100, f"x leaked: {x}"
assert result == [0, 1, 2], f"Got {result}"
print(f"  PASS: x={x}, result={result}")

# Test 4: Empty iterables
print("\nTest 4: Empty iterables")

# Test 4a: Empty outer iterable
g = (x*y for x in [] for y in range(3))
result = list(g)
assert result == [], f"Got {result}, expected []"
print(f"  PASS (empty outer): {result}")

# Test 4b: Empty inner iterable
g = (x*y for x in range(3) for y in [])
result = list(g)
assert result == [], f"Got {result}, expected []"
print(f"  PASS (empty inner): {result}")

# Test 4c: Both empty
g = (x*y for x in [] for y in [])
result = list(g)
assert result == [], f"Got {result}, expected []"
print(f"  PASS (both empty): {result}")

# Test 4d: Outer has elements, but condition filters all
g = (x for x in range(10) if x > 100)
result = list(g)
assert result == [], f"Got {result}, expected []"
print(f"  PASS (filtered to empty): {result}")

# Test 5: Mixed iterable types
print("\nTest 5: Mixed iterable types")

# Test 5a: List and tuple
g = (x+y for x in [1, 2, 3] for y in (10, 20))
result = list(g)
expected = [11, 21, 12, 22, 13, 23]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (list + tuple): {result}")

# Test 5b: Range and set
g = (x*y for x in range(3) for y in {1, 2})
result = sorted(list(g))
expected = sorted([0, 0, 1, 2, 2, 4])
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (range + set): {result}")

# Test 5c: Generator as source
def gen():
    yield 1
    yield 2
    yield 3

g = (x*2 for x in gen())
result = list(g)
expected = [2, 4, 6]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (generator source): {result}")

# Test 5d: Nested generators consuming nested generators
g1 = (x for x in range(3))
g2 = (x*y for x in g1 for y in range(2))
result = list(g2)
expected = [0, 0, 0, 1, 0, 2]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (nested gen): {result}")

# Test 6: Complex expressions
print("\nTest 6: Complex expressions")

# Test 6a: Function calls in expression
def f(x, y):
    return x * y + 1

g = (f(x, y) for x in range(3) for y in range(2))
result = list(g)
expected = [1, 1, 1, 2, 1, 3]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (function calls): {result}")

# Test 6b: Nested function calls
def g_func(y):
    return y * 2

g = (f(x, g_func(y)) for x in range(2) for y in range(2))
result = list(g)
expected = [1, 1, 1, 3]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (nested functions): {result}")

# Test 6c: Triple nested with conditions
g = (x+y+z for x in range(3) if x>0 for y in range(3) if y>0 for z in range(3) if z>0)
result = list(g)
expected = [3, 4, 4, 5, 4, 5, 5, 6]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (triple nested with conditions): {result}")

# Test 7: Edge cases
print("\nTest 7: Edge cases")

# Test 7a: Single element iterables
g = (x*y for x in [5] for y in [10])
result = list(g)
expected = [50]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (single elements): {result}")

# Test 7b: Condition that depends on both variables
g = (x*y for x in range(5) for y in range(5) if x+y == 4)
result = list(g)
expected = [0, 3, 4, 3, 0]
assert result == expected, f"Got {result}, expected {expected}"
print(f"  PASS (condition on both vars): {result}")

# Test 7c: Using same variable name (should be isolated)
x = 100
g = (x for x in [1, 2] for x in [10, 20])
result = list(g)
expected = [10, 20, 10, 20]
assert result == expected, f"Got {result}, expected {expected}"
assert x == 100, f"Outer x leaked: {x}"
print(f"  PASS (same var name): {result}")

# Test 8: Comparison with list comprehensions
print("\nTest 8: Comparison with list comprehensions")

# Test 8a: Double nested
gen_result = list((x*y for x in range(3) for y in range(3)))
list_result = [x*y for x in range(3) for y in range(3)]
assert gen_result == list_result, f"Gen: {gen_result}, List: {list_result}"
print(f"  PASS (double nested): {gen_result}")

# Test 8b: With conditions
gen_result = list((x*y for x in range(5) if x>2 for y in range(5) if y<3))
list_result = [x*y for x in range(5) if x>2 for y in range(5) if y<3]
assert gen_result == list_result, f"Gen: {gen_result}, List: {list_result}"
print(f"  PASS (with conditions): {gen_result}")

# Test 8c: Triple nested
gen_result = list((x+y+z for x in range(2) for y in range(2) for z in range(2)))
list_result = [x+y+z for x in range(2) for y in range(2) for z in range(2)]
assert gen_result == list_result, f"Gen: {gen_result}, List: {list_result}"
print(f"  PASS (triple nested): {gen_result}")

print("\n" + "="*50)
print("All comprehensive nested generator tests passed!")
print("="*50)
