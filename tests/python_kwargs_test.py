# Test **kwargs parameter and keyword arguments in Python syntax

# Test 1: Basic **kwargs
def func1(**kwargs):
    assert kwargs == {"x": 1, "y": 2}

func1(x=1, y=2)
print("Test 1 passed: Basic **kwargs")

# Test 2: **kwargs with dict unpacking
def func2(**kwargs):
    assert kwargs == {"a": 10, "b": 20}

d = {"a": 10, "b": 20}
func2(**d)
print("Test 2 passed: Dict unpacking with **d")

# Test 3: Mixed positional and **kwargs
def func3(x, y, **kwargs):
    assert x == 1
    assert y == 2
    assert kwargs == {"z": 3, "w": 4}

func3(1, 2, z=3, w=4)
print("Test 3 passed: Mixed positional and **kwargs")

# Test 4: **kwargs in function body
def func4(**options):
    return len(options)

result = func4(debug=True, verbose=False, level=5)
assert result == 3
print("Test 4 passed: Using **kwargs in function body")

# Test 5: Empty **kwargs
def func5(**kwargs):
    assert kwargs == {}

func5()
print("Test 5 passed: Empty **kwargs")

print("\nAll Python **kwargs tests passed!")
