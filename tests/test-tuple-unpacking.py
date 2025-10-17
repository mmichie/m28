print("Testing tuple unpacking in for loops...")

# Test with enumerate
items = ["a", "b", "c"]
for i, val in enumerate(items):
    print(f"{i}: {val}")

# Test with zip
names = ["Alice", "Bob", "Charlie"]
ages = [30, 25, 35]
for name, age in zip(names, ages):
    print(f"{name} is {age}")

# Test with list of tuples
pairs = [(1, "one"), (2, "two"), (3, "three")]
for num, word in pairs:
    print(f"{num} = {word}")

# Test with 3-way unpacking
print("\nTesting 3-way unpacking...")
triples = [(1, 2, 3), (4, 5, 6), (7, 8, 9)]
for a, b, c in triples:
    print(f"{a}, {b}, {c}")

print("\n✓ All tuple unpacking tests passed!")
