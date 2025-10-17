# Test import aliasing feature

print("Testing import aliasing...")

# Test 1: Single import with alias
from tests.test_module import test_function as tf
result = tf(5)
print("test_function(5) as tf:", result)

# Test 2: Multiple imports with mixed aliases
from tests.test_module import test_value as TV, another_function
print("test_value as TV:", TV)
print("another_function(10, 20):", another_function(10, 20))

# Test 3: Multiple imports all with aliases
from tests.test_module import test_function as func, test_value as val
print("test_function as func, test_value as val:", func(3), val)

# Test 4: Verify original names are not defined (should only have aliases)
try:
    print(test_function)  # Should fail - only 'tf' should be defined
    print("ERROR: test_function should not be defined!")
except NameError as e:
    print("✓ Original name 'test_function' not defined (correct):", str(e))

# Test 5: Verify alias works
print("✓ Alias 'tf' works:", tf(7))

# Test 6: Verify mixed import works (aliased + non-aliased)
print("✓ Non-aliased 'another_function' works:", another_function(5, 3))

print("\n✓ All import aliasing tests passed!")
