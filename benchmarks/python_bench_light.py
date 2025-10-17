# Lightweight Python Performance Benchmark
# For comparing Python 3 vs M28's Python frontend
#
# Usage:
#   time python3 benchmarks/python_bench_light.py
#   time ./bin/m28 benchmarks/python_bench_light.py

print("=== Python Benchmark (Light) ===")
print()

# Test 1: Fibonacci (recursive) - reduced iterations
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

print("Test 1: Recursive Fibonacci(20)")
result = fibonacci(20)
print("Result:", result)
print()

# Test 2: Factorial (iterative)
def factorial(n):
    result = 1
    for i in range(1, n + 1):
        result = result * i
    return result

print("Test 2: Iterative Factorial(100) - 100 iterations")
for i in range(100):
    result = factorial(100)
print("Result:", result)
print()

# Test 3: List comprehension
print("Test 3: List Comprehension - Sum of Squares (1-100)")
for i in range(100):
    squares = [x * x for x in range(100)]
    total = sum(squares)
print("Result:", total)
print()

# Test 4: Simple loops
print("Test 4: Nested Loops (50x50)")
total = 0
for i in range(50):
    for j in range(50):
        total = total + 1
print("Result:", total)
print()

# Test 5: Function calls
def add(a, b):
    return a + b

print("Test 5: Function Calls - 1000 iterations")
total = 0
for i in range(1000):
    total = add(total, 1)
print("Result:", total)
print()

# Test 6: Classes and methods
class Counter:
    def __init__(self, start):
        self.value = start

    def increment(self):
        self.value = self.value + 1
        return self.value

print("Test 6: Class Method Calls - 100 iterations")
c = Counter(0)
for i in range(100):
    result = c.increment()
print("Result:", result)
print()

print("=== Benchmark Complete ===")
