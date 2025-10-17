print("Testing starred unpacking in assignments...")

# Test 1: Basic starred unpacking
a, *b, c = [1, 2, 3, 4, 5]
print(f"a={a}, b={b}, c={c}")
assert a == 1, "a should be 1"
assert b == [2, 3, 4], "b should be [2, 3, 4]"
assert c == 5, "c should be 5"

# Test 2: Star at beginning
*head, tail = [1, 2, 3]
print(f"head={head}, tail={tail}")
assert head == [1, 2], "head should be [1, 2]"
assert tail == 3, "tail should be 3"

# Test 3: Star at end
first, *rest = [1, 2, 3, 4]
print(f"first={first}, rest={rest}")
assert first == 1, "first should be 1"
assert rest == [2, 3, 4], "rest should be [2, 3, 4]"

# Test 4: Star in middle
x, *middle, y = [1, 2, 3, 4, 5, 6]
print(f"x={x}, middle={middle}, y={y}")
assert x == 1, "x should be 1"
assert middle == [2, 3, 4, 5], "middle should be [2, 3, 4, 5]"
assert y == 6, "y should be 6"

# Test 5: Star with minimal values
p, *q, r = [1, 2]
print(f"p={p}, q={q}, r={r}")
assert p == 1, "p should be 1"
assert q == [], "q should be []"
assert r == 2, "r should be 2"

print("\n✓ All starred assignment tests passed!")
