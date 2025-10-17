print("Testing enumerate...")

# Basic enumerate
items = ['a', 'b', 'c']
for i, val in enumerate(items):
    print(f"{i}: {val}")

# Enumerate with start
print("\nEnumerate with start=1:")
for i, val in enumerate(items, 1):
    print(f"{i}: {val}")

print("\nTesting zip...")

# Basic zip
names = ['Alice', 'Bob', 'Charlie']
ages = [30, 25, 35]
for name, age in zip(names, ages):
    print(f"{name} is {age}")

# Zip with three lists
print("\nZip with 3 lists:")
scores = [90, 85, 95]
for name, age, score in zip(names, ages, scores):
    print(f"{name}, {age}, score: {score}")

print("\n✓ Done!")
