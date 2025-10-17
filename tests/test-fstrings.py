# Test f-string support

print("Testing f-strings...")

# Test 1: Simple f-string
name = "World"
greeting = f"Hello, {name}!"
print(greeting)

# Test 2: F-string with expression
x = 10
y = 5
result = f"The sum of {x} and {y} is {x + y}"
print(result)

# Test 3: F-string with multiple interpolations
a = 3
b = 4
c = 5
perimeter = f"Triangle with sides {a}, {b}, {c} has perimeter {a + b + c}"
print(perimeter)

# Test 4: F-string in function
def format_user(name, age):
    return f"User: {name}, Age: {age}"

user_info = format_user("Alice", 30)
print(user_info)

print("✓ F-string tests complete")
