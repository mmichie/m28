# Test Python builtin functions to see what's missing

print("Testing Python builtins...")

# Test basic ones we know exist
print("\n=== Testing known builtins ===")
print("enumerate:", list(enumerate([10, 20, 30])))
print("zip:", list(zip([1, 2], ['a', 'b'])))
print("all([True, True]):", all([True, True]))
print("any([False, False]):", any([False, False]))

# Test math functions
print("\n=== Testing math builtins ===")
try:
    print("abs(-5):", abs(-5))
except Exception as e:
    print("abs: MISSING -", e)

try:
    print("round(3.7):", round(3.7))
except Exception as e:
    print("round: MISSING -", e)

try:
    print("pow(2, 3):", pow(2, 3))
except Exception as e:
    print("pow: MISSING -", e)

try:
    print("min([3, 1, 2]):", min([3, 1, 2]))
except Exception as e:
    print("min: MISSING -", e)

try:
    print("max([3, 1, 2]):", max([3, 1, 2]))
except Exception as e:
    print("max: MISSING -", e)

try:
    print("sum([1, 2, 3]):", sum([1, 2, 3]))
except Exception as e:
    print("sum: MISSING -", e)

# Test character/string functions
print("\n=== Testing character/string builtins ===")
try:
    print("chr(65):", chr(65))
except Exception as e:
    print("chr: MISSING -", e)

try:
    print("ord('A'):", ord('A'))
except Exception as e:
    print("ord: MISSING -", e)

# Test number representation
print("\n=== Testing number representation builtins ===")
try:
    print("hex(255):", hex(255))
except Exception as e:
    print("hex: MISSING -", e)

try:
    print("bin(5):", bin(5))
except Exception as e:
    print("bin: MISSING -", e)

try:
    print("oct(8):", oct(8))
except Exception as e:
    print("oct: MISSING -", e)

# Test type/attribute functions
print("\n=== Testing type/attribute builtins ===")
try:
    print("callable(print):", callable(print))
except Exception as e:
    print("callable: MISSING -", e)

try:
    class Test:
        x = 10
    obj = Test()
    print("hasattr(obj, 'x'):", hasattr(obj, 'x'))
except Exception as e:
    print("hasattr: MISSING -", e)

try:
    print("getattr(obj, 'x'):", getattr(obj, 'x'))
except Exception as e:
    print("getattr: MISSING -", e)

try:
    setattr(obj, 'y', 20)
    print("setattr worked, obj.y:", obj.y)
except Exception as e:
    print("setattr: MISSING -", e)

try:
    print("delattr(obj, 'y')...")
    delattr(obj, 'y')
    print("delattr worked")
except Exception as e:
    print("delattr: MISSING -", e)

print("\n=== Test complete ===")
