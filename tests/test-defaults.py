# Test default parameter values

print("Testing default parameters...")

# Test 1: Simple default
def greet(name="World"):
    return f"Hello, {name}!"

result1 = greet()
print(f"greet() = {result1}")

result2 = greet("Python")
print(f"greet('Python') = {result2}")

# Test 2: Multiple parameters with defaults
def add(x, y=5):
    return x + y

result3 = add(10)
print(f"add(10) = {result3}")

result4 = add(10, 20)
print(f"add(10, 20) = {result4}")

# Test 3: All defaults
def multiply(x=2, y=3):
    return x * y

result5 = multiply()
print(f"multiply() = {result5}")

result6 = multiply(4)
print(f"multiply(4) = {result6}")

result7 = multiply(4, 5)
print(f"multiply(4, 5) = {result7}")

print("✓ Default parameters test complete")
