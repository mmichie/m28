# Test Python comprehensions

print("Testing comprehensions...")

nums = [1, 2, 3, 4, 5]

# List comprehension
doubled = [x * 2 for x in nums]
print(f"List comp: {doubled}")

# List comprehension with if
evens = [x for x in nums if x % 2 == 0]
print(f"List comp with if: {evens}")

# Dict comprehension
squares = {x: x * x for x in nums}
print(f"Dict comp: {squares}")

# Dict comprehension with if
even_squares = {x: x * x for x in nums if x % 2 == 0}
print(f"Dict comp with if: {even_squares}")

# Set comprehension
unique_mods = {x % 3 for x in range(10)}
print(f"Set comp: {unique_mods}")

# Set comprehension with if
even_mods = {x % 3 for x in range(10) if x % 2 == 0}
print(f"Set comp with if: {even_mods}")

# Nested list comprehensions
matrix = [[1, 2], [3, 4], [5, 6]]
flattened = [item for row in matrix for item in row]
print(f"Nested list comp: {flattened}")

# Nested with pairs
pairs = [(x, y) for x in [1, 2] for y in ["a", "b"]]
print(f"Nested pairs: {pairs}")

# Nested with condition
evens = [y for x in [[1, 2, 3], [4, 5, 6]] for y in x if y % 2 == 0]
print(f"Nested with condition: {evens}")

# Nested dict comprehension
nested_dict = {x: y for x in [1, 2] for y in [10, 20]}
print(f"Nested dict comp: {nested_dict}")

# Nested set comprehension
nested_set = {x + y for x in [1, 2] for y in [10, 20]}
print(f"Nested set comp: {nested_set}")

print("\n✓ All comprehension tests passed!")
