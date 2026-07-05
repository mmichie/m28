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

# Test 6: PEP 468 - kwargs preserve call-site keyword order (M28-acf)
d = dict(z=1, m=2, a=3, k=4, b=5, q=6, c=7, w=8)
assert list(d.keys()) == ["z", "m", "a", "k", "b", "q", "c", "w"]
print("Test 6 passed: dict() kwargs keep call-site order")

# Test 7: **kwargs parameter sees keys in call-site order
def kw_names(**kw):
    return list(kw.keys())

assert kw_names(z=1, a=2, m=3) == ["z", "a", "m"]
print("Test 7 passed: **kwargs keeps call-site order")

# Test 8: forwarding through **kw preserves order
def kw_forward(**kw):
    return kw_names(**kw)

assert kw_forward(w=9, e=8, r=7) == ["w", "e", "r"]
print("Test 8 passed: **kw forwarding keeps order")

# Test 9: named keywords and ** unpacking merge in source order
mid = {"b": 2}
assert list(dict(a=1, **mid, c=3).keys()) == ["a", "b", "c"]
print("Test 9 passed: named and ** merge in source order")

# Test 10: consumed names do not reach **kwargs; order kept for the rest
def kw_mixed(x, y=0, **rest):
    return list(rest.keys())

assert kw_mixed(1, m=5, y=2, k=6) == ["m", "k"]
print("Test 10 passed: consumed keywords removed, order kept")

# Test 11: functools.partial merges fixed and call kwargs in CPython order
import functools

pk = functools.partial(kw_names, q=1, z=2)
assert pk(a=3, z=9) == ["q", "z", "a"]
assert list(pk.keywords.keys()) == ["q", "z"]
print("Test 11 passed: partial kwargs merge order")

print("\nAll Python **kwargs tests passed!")
