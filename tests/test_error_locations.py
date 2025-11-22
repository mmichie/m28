# Comprehensive test for runtime error location tracking
# This file tests that various runtime errors show proper source location and context

# Test 1: NameError - undefined variable
def test_name_error():
    undefined_variable

# Test 2: TypeError - type mismatch
def test_type_error():
    x = "hello"
    result = x + 5

# Test 3: AttributeError - missing attribute
def test_attribute_error():
    x = 10
    x.nonexistent_method()

# Test 4: IndexError - index out of range
def test_index_error():
    lst = [1, 2, 3]
    value = lst[10]

# Test 5: KeyError - missing dictionary key
def test_key_error():
    d = {"a": 1, "b": 2}
    value = d["missing_key"]

# Test 6: ZeroDivisionError
def test_zero_division():
    result = 10 / 0

# Test 7: AssertionError
def test_assertion():
    x = 5
    assert x == 10

# Test 8: ValueError
def test_value_error():
    int("not_a_number")

# Run test_name_error to verify location tracking
test_name_error()
