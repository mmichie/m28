# Test Python lambda expressions

print("Testing lambda expressions...")

# Basic lambda
f = lambda x: x * 2
print(f"Basic: {f(5)}")

# No parameters
g = lambda: 42
print(f"No-param: {g()}")

# Multiple parameters
add = lambda x, y: x + y
print(f"Multi-param: {add(3, 4)}")

# Inline lambda
result = (lambda x: x + 1)(5)
print(f"Inline: {result}")

# Lambda with map
nums = [1, 2, 3, 4, 5]
doubled = list(map(lambda x: x * 2, nums))
print(f"Map: {doubled}")

# Lambda with filter
evens = list(filter(lambda x: x % 2 == 0, nums))
print(f"Filter: {evens}")

# Lambda returning lambda
make_adder = lambda n: lambda x: x + n
add_10 = make_adder(10)
print(f"Curried: {add_10(5)}")

# Lambda in defaultdict
from collections import defaultdict
dd = defaultdict(lambda: [])
dd["a"].append(1)
dd["a"].append(2)
print(f"Defaultdict: {dd['a']}")

print("\n✓ All lambda tests passed!")
