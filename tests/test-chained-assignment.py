# Test chained assignment

print("Testing chained assignment...")

# Test 1: Simple chained assignment
x = y = z = 0
print(f"x={x}, y={y}, z={z}")

# Test 2: Chained assignment with value
a = b = c = 42
print(f"a={a}, b={b}, c={c}")

# Test 3: Longer chain
one = two = three = four = 100
print(f"one={one}, two={two}, three={three}, four={four}")

# Test 4: Chained assignment with expression
m = n = 5 + 10
print(f"m={m}, n={n}")

# Test 5: Chained assignment with function call
def get_value():
    return 99

p = q = r = get_value()
print(f"p={p}, q={q}, r={r}")

# Test 6: Verify all variables have the same value
x = y = z = "hello"
print(f"All equal: {x == y and y == z}")

# Test 7: Chained with list
lst1 = lst2 = [1, 2, 3]
print(f"lst1={lst1}, lst2={lst2}")

print("✓ Chained assignment tests complete")
