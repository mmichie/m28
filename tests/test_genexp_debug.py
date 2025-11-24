# Debug nested generator expression
gen = (x * y for x in range(3) for y in range(3))
result = list(gen)
print(f"Result: {result}")
print(f"Expected: [0, 0, 0, 0, 1, 2, 0, 2, 4]")
