# Test multiple assignment / tuple unpacking

print("Testing multiple assignment...")

# Test 1: Basic tuple unpacking
x, y = 1, 2
print("x =", x, ", y =", y)

# Test 2: Three values
a, b, c = 10, 20, 30
print("a =", a, ", b =", b, ", c =", c)

# Test 3: Unpacking from tuple
coords = (100, 200)
p, q = coords
print("p =", p, ", q =", q)

# Test 4: Swap variables
m = 5
n = 10
m, n = n, m
print("After swap: m =", m, ", n =", n)

# Test 5: Unpacking from list
lst = [7, 8, 9]
first, second, third = lst
print("first =", first, ", second =", second, ", third =", third)

print("✓ Multiple assignment tests complete")
