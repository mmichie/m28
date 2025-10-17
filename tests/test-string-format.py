# Test string.format() method with format specifications

print("Testing string.format() method...")

# Test 1: Basic formatting with .4f
elapsed = 1.23456789
result = "Time: {:.4f} seconds".format(elapsed)
print(result)

# Test 2: .6f precision
elapsed2 = 0.0000123456
result2 = "Per iteration: {:.6f} seconds".format(elapsed2)
print(result2)

# Test 3: Multiple arguments
x = 10
y = 20.5
result3 = "x={}, y={:.2f}".format(x, y)
print(result3)

# Test 4: Positional arguments
result4 = "{0} + {1} = {2}".format(5, 3, 8)
print(result4)

# Test 5: Integer formatting
count = 1000
result5 = "Count: {:.0f}".format(count)
print(result5)

# Test 6: Scientific notation
big = 123456789.0
result6 = "Scientific: {:.2e}".format(big)
print(result6)

# Test 7: Width and alignment
val = 42
result7 = "Value: {:>10.2f}".format(val)
print(result7)

print("✓ All string.format() tests complete")
