# Test tuple unpacking in with statements
# Python allows: with context() as (a, b): ...

print("Test 1: Basic with statement (no unpacking)")
class SimpleContext:
    def __enter__(self):
        return 42

    def __exit__(self, exc_type, exc_val, exc_tb):
        return False

with SimpleContext() as value:
    assert value == 42, f"Expected 42, got {value}"
print("✓ Test 1 passed")

print("\nTest 2: With statement with tuple unpacking")
class DualContext:
    def __enter__(self):
        return (1, 2)

    def __exit__(self, exc_type, exc_val, exc_tb):
        return False

try:
    with DualContext() as (a, b):
        print(f"a={a}, b={b}")
        assert a == 1 and b == 2, f"Expected a=1, b=2, got a={a}, b={b}"
    print("✓ Test 2 passed")
except SyntaxError as e:
    print(f"✗ Test 2 failed with SyntaxError: {e}")
    import sys
    sys.exit(1)
except Exception as e:
    print(f"✗ Test 2 failed: {e}")
    import sys
    sys.exit(1)

print("\nTest 3: With statement with triple unpacking")
class TripleContext:
    def __enter__(self):
        return (1, 2, 3)

    def __exit__(self, exc_type, exc_val, exc_tb):
        return False

try:
    with TripleContext() as (a, b, c):
        print(f"a={a}, b={b}, c={c}")
        assert a == 1 and b == 2 and c == 3, f"Expected a=1, b=2, c=3, got a={a}, b={b}, c={c}"
    print("✓ Test 3 passed")
except SyntaxError as e:
    print(f"✗ Test 3 failed with SyntaxError: {e}")
    import sys
    sys.exit(1)
except Exception as e:
    print(f"✗ Test 3 failed: {e}")
    import sys
    sys.exit(1)

print("\n✅ All with statement unpacking tests passed!")
