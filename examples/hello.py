# Simple Python program to test Phase E integration

# Variables and assignment
x = 42
name = "Python"

# Function definition
def greet(who):
    return "Hello, " + who + "!"

# Function call
message = greet(name)
print(message)

# List comprehension
squares = [i * i for i in range(5)]
print("Squares:")
print(squares)

# Control flow
if x > 40:
    print("x is greater than 40")
else:
    print("x is small")

# For loop
total = 0
for i in range(10):
    total = total + i

print("Total:")
print(total)
