print("Testing simple comprehensions...")

# List comprehension
nums = [1, 2, 3, 4, 5]
doubled = [x * 2 for x in nums]
print(f"List: {doubled}")

# Set comprehension
mods = {x % 3 for x in range(10)}
print(f"Set: {mods}")

print("Done!")
