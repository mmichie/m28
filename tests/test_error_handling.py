# Comprehensive Error Handling Test Suite
# This file tests M28's error reporting for common Python syntax and runtime errors
#
# To test individual error cases, uncomment ONE test section at a time and run:
#   ./bin/m28 tests/test_error_handling.py
#
# This allows manual verification that error messages are:
# 1. Clear and helpful
# 2. Show correct line numbers and source context
# 3. Include caret (^) indicators at the right position
# 4. Match Python's error reporting style

print("Error Handling Test Suite")
print("=" * 60)

# ==============================================================================
# TOKENIZATION ERRORS
# ==============================================================================

# Test 1: Unterminated single-quoted string
# Uncomment to test:
# x = 'this string has no ending quote
# Expected: TokenizationError with helpful message about missing closing quote

# Test 2: Unterminated double-quoted string
# Uncomment to test:
# message = "hello world
# Expected: TokenizationError suggesting to add closing "

# Test 3: Unterminated triple-quoted string
# Uncomment to test:
# doc = """This is a docstring
# that never ends
# Expected: TokenizationError about triple-quoted string

# Test 4: Unterminated f-string
# Uncomment to test:
# name = "Alice
# greeting = f"Hello {name}
# Expected: TokenizationError on the unterminated string

# Test 5: Invalid escape sequence in string
# Note: This might be handled differently depending on implementation
# Uncomment to test:
# path = "C:\new\test"  # \n and \t are escape sequences
# Expected: Either works or shows warning about escape sequences

# Test 6: Bad indentation - inconsistent use of tabs/spaces
# Uncomment to test:
# def foo():
#     x = 1
#	    y = 2  # This line uses a tab
# Expected: IndentationError about inconsistent whitespace

# Test 7: Unexpected dedent
# Uncomment to test:
# def bar():
#     x = 1
#   y = 2  # Bad dedent - doesn't match any previous level
# Expected: IndentationError about dedentation

# Test 8: Invalid number format
# Uncomment to test:
# x = 1.5e  # Incomplete exponent
# Expected: TokenizationError about invalid number

# Test 9: Invalid hex number
# Uncomment to test:
# x = 0xGHI  # G, H, I are not hex digits
# Expected: TokenizationError about invalid hex literal

# Test 10: Multiple dots in number
# Uncomment to test:
# x = 1.2.3
# Expected: TokenizationError or ParseError

# ==============================================================================
# PARSE ERRORS
# ==============================================================================

# Test 11: Missing colon in if statement
# Uncomment to test:
# if True
#     print("hello")
# Expected: ParseError "Expected COLON after 'if' condition"

# Test 12: Missing colon in for loop
# Uncomment to test:
# for i in range(10)
#     print(i)
# Expected: ParseError "Expected COLON"

# Test 13: Missing colon in while loop
# Uncomment to test:
# while True
#     break
# Expected: ParseError "Expected COLON"

# Test 14: Missing colon in function definition
# Uncomment to test:
# def my_func(x, y)
#     return x + y
# Expected: ParseError "Expected COLON"

# Test 15: Missing colon in class definition
# Uncomment to test:
# class MyClass
#     pass
# Expected: ParseError "Expected COLON"

# Test 16: Unclosed parenthesis
# Uncomment to test:
# result = (1 + 2 + 3
# print(result)
# Expected: ParseError about unclosed parenthesis

# Test 17: Unclosed bracket
# Uncomment to test:
# my_list = [1, 2, 3
# print(my_list)
# Expected: ParseError about missing RBRACKET

# Test 18: Unclosed brace
# Uncomment to test:
# my_dict = {"a": 1, "b": 2
# Expected: ParseError about missing RBRACE

# Test 19: Mismatched closing delimiter
# Uncomment to test:
# data = [1, 2, 3}
# Expected: ParseError about mismatched brackets/braces

# Test 20: Missing closing quote in dict key
# Uncomment to test:
# config = {"name: "value"}
# Expected: TokenizationError about unterminated string

# Test 21: Invalid assignment target
# Uncomment to test:
# 42 = x
# Expected: ParseError "Invalid assignment target"

# Test 22: Invalid augmented assignment
# Uncomment to test:
# x + y = 10
# Expected: ParseError about invalid assignment

# Test 23: Standalone expression with no effect
# Note: Python allows this, but it's suspicious
# result = 5
# result
# This should work, but maybe with a warning

# Test 24: Missing value in assignment
# Uncomment to test:
# x =
# Expected: ParseError about unexpected end or newline

# Test 25: Double operators
# Uncomment to test:
# x = 5 + + 3
# Expected: ParseError or might work as unary +

# Test 26: Missing argument in function call
# Uncomment to test:
# def foo(a, b): return a + b
# result = foo(1, )
# Expected: ParseError or might work depending on implementation

# Test 27: Invalid keyword argument
# Uncomment to test:
# def bar(x): return x * 2
# bar(123=456)
# Expected: ParseError about invalid keyword argument

# Test 28: Missing except clause
# Uncomment to test:
# try:
#     x = 1
# Expected: ParseError "Expected 'except' or 'finally' after 'try'"

# Test 29: Invalid decorator syntax
# Uncomment to test:
# @
# def decorated(): pass
# Expected: ParseError about decorator

# Test 30: Multiple else clauses
# Uncomment to test:
# if True:
#     x = 1
# else:
#     x = 2
# else:
#     x = 3
# Expected: ParseError about duplicate else

# ==============================================================================
# RUNTIME ERRORS
# ==============================================================================

# Test 31: NameError - undefined variable
# Uncomment to test:
# print(undefined_variable)
# Expected: NameError with helpful message and source context

# Test 32: NameError - typo in variable name
# Uncomment to test:
# message = "hello"
# print(mesage)  # Typo: 'mesage' vs 'message'
# Expected: NameError, ideally with suggestion "Did you mean 'message'?"

# Test 33: TypeError - wrong number of arguments
# Uncomment to test:
# def greet(name): return f"Hello {name}"
# greet()  # Missing required argument
# Expected: TypeError about missing argument

# Test 34: TypeError - too many arguments
# Uncomment to test:
# def add(a, b): return a + b
# add(1, 2, 3)  # Too many arguments
# Expected: TypeError about too many arguments

# Test 35: TypeError - cannot concatenate str and int
# Uncomment to test:
# result = "number: " + 42
# Expected: TypeError about incompatible types

# Test 36: TypeError - unsupported operation
# Uncomment to test:
# result = "hello" - "world"
# Expected: TypeError about unsupported operation

# Test 37: TypeError - not callable
# Uncomment to test:
# x = 42
# x()
# Expected: TypeError "int object is not callable"

# Test 38: AttributeError - missing attribute
# Uncomment to test:
# class Person:
#     def __init__(self, name):
#         self.name = name
# p = Person("Alice")
# print(p.age)  # 'age' attribute doesn't exist
# Expected: AttributeError with helpful message

# Test 39: AttributeError - typo in attribute name
# Uncomment to test:
# my_list = [1, 2, 3]
# my_list.apend(4)  # Typo: 'apend' vs 'append'
# Expected: AttributeError, ideally with suggestion "Did you mean 'append'?"

# Test 40: KeyError - missing dictionary key
# Uncomment to test:
# config = {"host": "localhost", "port": 8080}
# db = config["database"]  # Key doesn't exist
# Expected: KeyError with the missing key

# Test 41: IndexError - list index out of range
# Uncomment to test:
# items = [1, 2, 3]
# value = items[10]
# Expected: IndexError about index out of range

# Test 42: IndexError - negative index too large
# Uncomment to test:
# items = [1, 2, 3]
# value = items[-10]
# Expected: IndexError

# Test 43: ZeroDivisionError
# Uncomment to test:
# x = 10
# y = 0
# result = x / y
# Expected: ZeroDivisionError with source context

# Test 44: ZeroDivisionError in complex expression
# Uncomment to test:
# result = (100 + 50) / (10 - 10)
# Expected: ZeroDivisionError

# Test 45: ValueError - invalid conversion
# Uncomment to test:
# number = int("not a number")
# Expected: ValueError about invalid literal

# Test 46: ValueError - invalid value for operation
# Uncomment to test:
# import math
# result = math.sqrt(-1)
# Expected: ValueError (or complex number if supported)

# Test 47: ImportError - module not found
# Uncomment to test:
# import nonexistent_module
# Expected: ImportError with helpful message

# Test 48: ImportError - name not in module
# Uncomment to test:
# from math import nonexistent_function
# Expected: ImportError about missing name

# Test 49: RecursionError - infinite recursion
# Uncomment to test:
# def infinite():
#     return infinite()
# infinite()
# Expected: RecursionError with stack trace

# Test 50: AssertionError
# Uncomment to test:
# assert False, "This assertion failed"
# Expected: AssertionError with custom message

# ==============================================================================
# COMPLEX ERROR SCENARIOS
# ==============================================================================

# Test 51: Error in nested function call
# Uncomment to test:
# def outer():
#     def inner():
#         return undefined_var
#     return inner()
# outer()
# Expected: NameError with stack trace showing call chain

# Test 52: Error in list comprehension
# Uncomment to test:
# result = [x * factor for x in range(10)]  # 'factor' is undefined
# Expected: NameError in comprehension context

# Test 53: Error in dictionary comprehension
# Uncomment to test:
# result = {k: v * multiplier for k, v in {"a": 1}.items()}
# Expected: NameError

# Test 54: Error in generator expression
# Uncomment to test:
# gen = (x * scale for x in range(10))  # 'scale' is undefined
# list(gen)  # Error happens when consuming generator
# Expected: NameError

# Test 55: Error in lambda
# Uncomment to test:
# f = lambda x: x + undefined
# f(5)
# Expected: NameError

# Test 56: Error in f-string expression
# Uncomment to test:
# name = "Alice"
# greeting = f"Hello {name.upper()} {age}"  # 'age' is undefined
# Expected: NameError in f-string context

# Test 57: Error in f-string format spec
# Uncomment to test:
# value = 42
# formatted = f"{value:{width}}"  # 'width' is undefined
# Expected: NameError

# Test 58: Multiple errors in sequence
# Uncomment to test:
# try:
#     x = undefined1
# except NameError:
#     y = undefined2  # Second error in except block
# Expected: NameError for undefined2

# Test 59: Error in class method
# Uncomment to test:
# class Calculator:
#     def divide(self, a, b):
#         return a / b
# calc = Calculator()
# calc.divide(10, 0)
# Expected: ZeroDivisionError with stack trace

# Test 60: Error in property getter
# Uncomment to test:
# class Person:
#     @property
#     def age(self):
#         return self._age  # _age doesn't exist
# p = Person()
# print(p.age)
# Expected: AttributeError

# ==============================================================================
# EDGE CASES
# ==============================================================================

# Test 61: Very long line with error at end
# Uncomment to test:
# result = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + undefined_var
# Expected: Error with caret pointing to end of line

# Test 62: Error on first line
# Uncomment to test: (Move to line 1)
# undefined_on_line_1
# Expected: Error showing line 1

# Test 63: Error with unicode characters
# Uncomment to test:
# message = "Hello 世界"
# print(undefined_变量)
# Expected: NameError with correct position despite unicode

# Test 64: Error in multiline string
# This should work fine:
# text = """
# Line 1
# Line 2
# Line 3
# """
# print(text)

# Test 65: Error after multiline string
# Uncomment to test:
# doc = """
# Multiline
# String
# """
# print(undefined_after_multiline)
# Expected: NameError with correct line number

# ==============================================================================
# ERROR RECOVERY AND MULTIPLE ERRORS
# ==============================================================================

# Test 66: Multiple parse errors in sequence
# Uncomment to test:
# if True
#     x = 1
# if False
#     y = 2
# Expected: Should report first error (or maybe both with error recovery)

# Test 67: Parse error followed by runtime error
# Uncomment to test:
# x = 10
# if x > 5
# Expected: ParseError stops before runtime

# Test 68: Syntax error in imported module
# This would require creating a separate .py file with errors

# Test 69: Error in eval/exec
# Uncomment to test:
# code = "x = undefined"
# eval(code)
# Expected: NameError

# Test 70: Error with traceback through multiple files
# This would require multiple files

# ==============================================================================
# VERIFICATION TESTS (These should all PASS)
# ==============================================================================

print("\nRunning verification tests (should all pass)...")

# Test that basic operations work correctly
x = 10
y = 20
assert x + y == 30, "Addition should work"
print("✓ Basic arithmetic works")

# Test that strings work
message = "Hello, World!"
assert len(message) == 13, "String length should work"
print("✓ Strings work")

# Test that lists work
items = [1, 2, 3, 4, 5]
assert len(items) == 5, "List should have 5 items"
assert items[0] == 1, "List indexing should work"
print("✓ Lists work")

# Test that dicts work
person = {"name": "Alice", "age": 30}
assert person["name"] == "Alice", "Dict access should work"
print("✓ Dictionaries work")

# Test that functions work
def double(x):
    return x * 2

assert double(5) == 10, "Functions should work"
print("✓ Functions work")

# Test that exception handling works
try:
    result = 10 / 2
    assert result == 5, "Division should work"
except Exception as e:
    assert False, f"Should not raise exception: {e}"
print("✓ Exception handling works")

# Test that proper errors ARE caught when they should be
error_caught = False
try:
    x = 1 / 0
except ZeroDivisionError:
    error_caught = True
assert error_caught, "Should catch ZeroDivisionError"
print("✓ Exceptions are properly caught")

print("\n" + "=" * 60)
print("Verification tests completed successfully!")
print("\nTo test error cases:")
print("1. Uncomment ONE error test section at a time")
print("2. Run: ./bin/m28 tests/test_error_handling.py")
print("3. Verify the error message is clear and helpful")
print("4. Check that line numbers and source context are correct")
print("5. Look for caret (^) indicators pointing to the error")
print("=" * 60)
