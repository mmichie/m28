print("Testing basic comprehensions...")

# Basic list comprehension
nums = [1, 2, 3, 4, 5]
doubled = [x * 2 for x in nums]
print(f"Basic list comp: {doubled}")

# Dict comprehension
squares = {x: x**2 for x in nums}
print(f"Dict comp: {squares}")

# Set comprehension
unique_mods = {x % 3 for x in range(10)}
print(f"Set comp: {unique_mods}")

print("\n✓ Basic comprehension tests completed!")
