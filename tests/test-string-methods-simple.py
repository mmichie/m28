# Test new string methods - simplified

print("Testing new string methods...")

# Test removeprefix
print("\nTest removeprefix:")
s = "TestHook"
result = s.removeprefix("Test")
print("Result:", result)

# Test removesuffix
print("\nTest removesuffix:")
s = "image.png"
result = s.removesuffix(".png")
print("Result:", result)

# Test partition
print("\nTest partition:")
s = "hello world python"
result = s.partition(" ")
print("Result:", result)

# Test rpartition
print("\nTest rpartition:")
s = "a.b.c.d"
result = s.rpartition(".")
print("Result:", result)

# Test expandtabs
print("\nTest expandtabs:")
s = "hello\tworld"
result = s.expandtabs(4)
print("Result:", result)

# Test zfill
print("\nTest zfill:")
s = "42"
result = s.zfill(5)
print("Result:", result)

s2 = "-42"
result2 = s2.zfill(5)
print("Result with sign:", result2)

# Test translate
print("\nTest translate:")
table = {97: "X", 101: None}  # a->X, e->delete
s = "hello world"
result = s.translate(table)
print("Result:", result)

print("\n✓ All string method tests completed!")
