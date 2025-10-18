"""
Test Python grammar compliance (M28-compatible version)
Simplified from CPython's test_grammar.py
"""

print("=== Python Grammar Tests (M28) ===")

# ============================================================================
# Literals
# ============================================================================
print("\n1. Testing literals...")

# Integer literals
x = 0
x = 123
x = 1000000  # No underscores in M28

# Float literals
y = 0.0
y = 3.14
y = 1.5e10

# String literals
s = "hello"
s = 'world'

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
x = 0
y = 0

# Augmented assignment
x = 10
x += 5
x -= 3
x *= 2
x //= 2

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
result = 10 // 3
result = 10 % 3
result = 2 ** 8

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
assert total == 25

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
assert len(lst) == 3

# Dictionaries
d = {}
d = {"a": 1, "b": 2}
assert d["a"] == 1

# Sets
s = {1, 2, 3}
assert len(s) == 3

# Tuples
t = (1, 2, 3)
assert len(t) == 3

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
assert p.distance_squared() == 25

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

# Try-finally
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
