# Test control flow (break/continue/return) in finally blocks
# This is legal in Python and should work

print("Test 1: Return in finally block")
def test_return_finally():
    try:
        return 1
    finally:
        return 2  # This should override the return value

result = test_return_finally()
assert result == 2, f"Expected 2, got {result}"
print(f"✓ Test 1 passed: {result}")

print("\nTest 2: Break in finally block")
result = []
for i in range(5):
    try:
        result.append(i)
    finally:
        if i == 2:
            break
print(f"Result: {result}")
assert result == [0, 1, 2], f"Expected [0, 1, 2], got {result}"
print("✓ Test 2 passed")

print("\nTest 3: Continue in finally block")
result = []
for i in range(5):
    try:
        result.append(f"try-{i}")
    finally:
        if i % 2 == 0:
            continue
        result.append(f"finally-{i}")
print(f"Result: {result}")
expected = ["try-0", "try-1", "finally-1", "try-2", "try-3", "finally-3", "try-4"]
assert result == expected, f"Expected {expected}, got {result}"
print("✓ Test 3 passed")

print("\nTest 4: Return overriding exception in finally")
def test_exception_override():
    try:
        raise ValueError("error")
    finally:
        return "overridden"

result = test_exception_override()
assert result == "overridden", f"Expected 'overridden', got {result}"
print(f"✓ Test 4 passed: {result}")

print("\n✅ All finally control flow tests passed!")
