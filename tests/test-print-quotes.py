# Test that print doesn't add extra quotes around strings

print("Testing print without quotes...")
print("Hello, World!")
print("x =", 10)
print("Time:", 1.234, "seconds")

# Multiple arguments
a = "foo"
b = "bar"
print(a, b)

# Numbers shouldn't be affected
print(42)
print(3.14159)

print("✓ Print test complete")
