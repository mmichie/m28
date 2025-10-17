print("Testing comprehensions...")

# Basic list comprehension (should work)
nums = [1, 2, 3, 4, 5]
doubled = [x * 2 for x in nums]
print(f"Basic list comp: {doubled}")

# List comprehension with if
evens = [x for x in nums if x % 2 == 0]
print(f"List comp with if: {evens}")

# List comprehension with ternary
signs = [1 if x > 0 else -1 for x in [-2, -1, 0, 1, 2]]
print(f"List comp with ternary: {signs}")

# Dict comprehension
squares = {x: x**2 for x in nums}
print(f"Dict comp: {squares}")

# Dict comprehension with if
even_squares = {x: x**2 for x in nums if x % 2 == 0}
print(f"Dict comp with if: {even_squares}")

# Set comprehension
unique_mods = {x % 3 for x in range(10)}
print(f"Set comp: {unique_mods}")

# Nested comprehension
matrix = [[1, 2], [3, 4], [5, 6]]
flattened = [item for row in matrix for item in row]
print(f"Nested comp: {flattened}")

# Comprehension with multiple variables
pairs = [(x, y) for x in [1, 2] for y in ['a', 'b']]
print(f"Multi-var comp: {pairs}")

print("\n✓ Comprehension tests completed!")
