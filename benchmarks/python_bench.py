# Python Performance Benchmark
# Can run on both Python 3 and M28's Python frontend
#
# Usage:
#   python3 benchmarks/python_bench.py
#   ./bin/m28 benchmarks/python_bench.py

import time

print("=== Python Performance Benchmark ===")
print()

# Fibonacci (recursive) - tests function call overhead
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

print("Test 1: Recursive Fibonacci(25)")
start = time.time()
result = fibonacci(25)
elapsed = time.time() - start
print("Result:", result)
print("Time: {:.4f} seconds".format(elapsed))
print()

# Factorial (iterative) - tests loops and multiplication
def factorial(n):
    result = 1
    for i in range(1, n + 1):
        result = result * i
    return result

print("Test 2: Iterative Factorial(100)")
start = time.time()
for i in range(10000):
    result = factorial(100)
elapsed = time.time() - start
print("Iterations: 10000")
print("Time: {:.4f} seconds".format(elapsed))
print("Per iteration: {:.6f} seconds".format(elapsed / 10000))
print()

# Sum of squares - tests list comprehensions
print("Test 3: List Comprehension - Sum of Squares (1-1000)")
start = time.time()
for i in range(1000):
    squares = [x * x for x in range(1000)]
    total = sum(squares)
elapsed = time.time() - start
print("Iterations: 1000")
print("Time: {:.4f} seconds".format(elapsed))
print("Per iteration: {:.6f} seconds".format(elapsed / 1000))
print()

# Nested loops - tests loop overhead and arithmetic
print("Test 4: Nested Loops (100x100)")
start = time.time()
for i in range(100):
    total = 0
    for j in range(100):
        for k in range(100):
            total = total + 1
elapsed = time.time() - start
print("Time: {:.4f} seconds".format(elapsed))
print()

# Function calls - tests function call overhead
def add(a, b):
    return a + b

print("Test 5: Simple Function Calls")
start = time.time()
total = 0
for i in range(100000):
    total = add(total, 1)
elapsed = time.time() - start
print("Iterations: 100000")
print("Time: {:.4f} seconds".format(elapsed))
print("Per iteration: {:.6f} seconds".format(elapsed / 100000))
print()

# Classes and methods - tests OOP overhead
class Counter:
    def __init__(self, start):
        self.value = start

    def increment(self):
        self.value = self.value + 1
        return self.value

print("Test 6: Class Method Calls")
start = time.time()
c = Counter(0)
for i in range(10000):
    c.increment()
elapsed = time.time() - start
print("Iterations: 10000")
print("Time: {:.4f} seconds".format(elapsed))
print("Per iteration: {:.6f} seconds".format(elapsed / 10000))
print()

print("=== Benchmark Complete ===")
