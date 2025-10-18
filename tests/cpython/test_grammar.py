"""
Test Python grammar compliance
Adapted from CPython's Lib/test/test_grammar.py for M28
Focuses on core syntax features
"""

print("=== Python Grammar Tests ===")

# ============================================================================
# Literals
# ============================================================================
print("\n1. Testing literals...")

# Integer literals
x = 0
x = 123
x = 1_000_000  # Underscore in numbers

# Binary, octal, hex literals
x = 0b1010  # 10
x = 0o77    # 63
x = 0xFF    # 255

# Float literals
y = 0.0
y = 3.14
y = 1.5e10
y = 1.5e-10

# String literals
s = "hello"
s = 'world'
s = """multi
line"""

# Boolean and None
b = True
b = False
n = None

print("✓ Literals passed")

# ============================================================================
# Variables and Assignment
# ============================================================================
print("\n2. Testing assignment...")

# Simple assignment
a = 1
b = 2
c = a + b

# Multiple assignment
x, y = 1, 2
x, y, z = 1, 2, 3

# Augmented assignment
x = 10
x += 5   # 15
x -= 3   # 12
x *= 2   # 24
x //= 2  # 12

print("✓ Assignment passed")

# ============================================================================
# Expressions
# ============================================================================
print("\n3. Testing expressions...")

# Arithmetic
result = 5 + 3
result = 5 - 3
result = 5 * 3
result = 10 / 2
result = 10 // 3  # Floor division
result = 10 % 3   # Modulo
result = 2 ** 8   # Power

# Comparison
result = 5 > 3
result = 5 < 10
result = 5 >= 5
result = 5 <= 5
result = 5 == 5
result = 5 != 3

# Logical
result = True and False
result = True or False
result = not True

# Membership
result = 1 in [1, 2, 3]
result = 4 not in [1, 2, 3]

print("✓ Expressions passed")

# ============================================================================
# Control Flow - If
# ============================================================================
print("\n4. Testing if statements...")

# Simple if
if True:
    x = 1

# If-else
if False:
    x = 1
else:
    x = 2

# If-elif-else
x = 15
if x < 10:
    result = "small"
elif x < 20:
    result = "medium"
else:
    result = "large"

assert result == "medium"

print("✓ If statements passed")

# ============================================================================
# Control Flow - Loops
# ============================================================================
print("\n5. Testing loops...")

# For loop
total = 0
for i in range(5):
    total += i
assert total == 10

# For loop with list
items = [1, 2, 3]
count = 0
for item in items:
    count += 1
assert count == 3

# While loop
x = 0
while x < 5:
    x += 1
assert x == 5

# Break
for i in range(10):
    if i == 5:
        break
assert i == 5

# Continue
total = 0
for i in range(10):
    if i % 2 == 0:
        continue
    total += i
assert total == 25  # 1+3+5+7+9

print("✓ Loops passed")

# ============================================================================
# Functions
# ============================================================================
print("\n6. Testing functions...")

# Simple function
def simple():
    return 42

assert simple() == 42

# Function with parameters
def add(a, b):
    return a + b

assert add(3, 4) == 7

# Function with default parameters
def greet(name="World"):
    return "Hello, " + name

assert greet() == "Hello, World"
assert greet("Python") == "Hello, Python"

# Nested function
def outer():
    def inner():
        return 10
    return inner()

assert outer() == 10

# Lambda
square = lambda x: x * x
assert square(5) == 25

print("✓ Functions passed")

# ============================================================================
# Data Structures
# ============================================================================
print("\n7. Testing data structures...")

# Lists
lst = []
lst = [1, 2, 3]
lst = [1, 2, 3, 4, 5]
assert len(lst) == 5
assert lst[0] == 1
assert lst[-1] == 5

# Dictionaries
d = {}
d = {"a": 1, "b": 2}
assert d["a"] == 1
assert len(d) == 2

# Sets
s = {1, 2, 3}
assert len(s) == 3
assert 1 in s
assert 4 not in s

# Tuples
t = (1, 2, 3)
assert len(t) == 3
assert t[0] == 1

print("✓ Data structures passed")

# ============================================================================
# Comprehensions
# ============================================================================
print("\n8. Testing comprehensions...")

# List comprehension
squares = [x * x for x in range(5)]
assert squares == [0, 1, 4, 9, 16]

# List comprehension with condition
evens = [x for x in range(10) if x % 2 == 0]
assert evens == [0, 2, 4, 6, 8]

# Dict comprehension
d = {x: x * x for x in range(5)}
assert d[2] == 4
assert d[4] == 16

# Set comprehension
s = {x % 3 for x in range(10)}
assert len(s) == 3

print("✓ Comprehensions passed")

# ============================================================================
# Classes
# ============================================================================
print("\n9. Testing classes...")

# Simple class
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def distance_squared(self):
        return self.x * self.x + self.y * self.y

p = Point(3, 4)
assert p.x == 3
assert p.y == 4
assert p.distance_squared() == 25

# Class with methods
class Counter:
    def __init__(self, start):
        self.value = start

    def increment(self):
        self.value += 1
        return self.value

c = Counter(0)
assert c.increment() == 1
assert c.increment() == 2

print("✓ Classes passed")

# ============================================================================
# Exception Handling
# ============================================================================
print("\n10. Testing exceptions...")

# Try-except
caught = False
try:
    x = 1 / 0
except:
    caught = True
assert caught

# Try-except-finally
executed = False
try:
    x = 1
finally:
    executed = True
assert executed

print("✓ Exceptions passed")

# ============================================================================
# Summary
# ============================================================================
print("\n" + "="*60)
print("✅ All Python Grammar Tests Passed!")
print("="*60)
print("\nFeatures tested:")
print("  ✓ Literals (int, float, str, bool, None)")
print("  ✓ Variables and assignment")
print("  ✓ Expressions and operators")
print("  ✓ If/elif/else statements")
print("  ✓ For and while loops")
print("  ✓ Functions and lambdas")
print("  ✓ Data structures (list, dict, set, tuple)")
print("  ✓ Comprehensions")
print("  ✓ Classes and methods")
print("  ✓ Exception handling")
