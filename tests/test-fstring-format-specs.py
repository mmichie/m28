# Test f-strings with format specifications

print("Testing f-strings with format specs...")

# Test 1: Basic f-string with .4f
elapsed = 1.23456789
result = f"Time: {elapsed:.4f} seconds"
print(result)

# Test 2: .6f precision
elapsed2 = 0.0000123456
result2 = f"Per iteration: {elapsed2:.6f} seconds"
print(result2)

# Test 3: Multiple values
x = 10
y = 20.5
result3 = f"x={x}, y={y:.2f}"
print(result3)

# Test 4: Integer formatting
count = 1000
result4 = f"Count: {count:.0f}"
print(result4)

print("✓ All f-string format spec tests complete")
