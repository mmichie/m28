# Pythonic Function Call Syntax

## Overview

M28 now supports **Pythonic function call syntax** `name(args)` alongside traditional S-expressions!

This makes M28 **perfect for shell one-liners** while maintaining the power of S-expressions for metaprogramming.

## What Changed

### Before (S-expression only)
```bash
m28 -e '(print (sum [(* x x) for x in (range 10)]))'
```

### After (Pythonic style)
```bash
m28 -e 'print(sum([x*x for x in range(10)]))'
```

## How It Works

The parser automatically transforms `name(args)` into `(name args)`:

```python
# Pythonic style (NEW!)
print("hello")              → (print "hello")
len([1, 2, 3])              → (len [1, 2, 3])
sum([x for x in range(10)]) → (sum [x for x in (range 10)])

# S-expression style (still works!)
(print "hello")
(len [1, 2, 3])
(sum [x for x in (range 10)])
```

## Whitespace Sensitivity

**Important**: No whitespace between function name and `(` means Pythonic sugar:

```python
print("hi")   # Pythonic: transforms to (print "hi")
print ("hi")  # S-expression: stays as (print "hi")
```

This lets you use both styles in the same program!

## Examples

### Shell One-Liners
```bash
# Calculate sum of squares
m28 -e 'print(sum([x*x for x in range(10)]))'
# Output: 285

# Find maximum value
m28 -e 'print(max([3, 1, 4, 1, 5, 9]))'
# Output: 9

# Sort and reverse
m28 -e 'print(list(reversed(sorted([3,1,4,1,5]))))'
# Output: [5, 4, 3, 1, 1]
```

### In Files
```python
# Variable assignment
x = [1, 2, 3, 4, 5]

# Pythonic calls
print(len(x))
print(sum(x))
print(max(x))

# Mix with S-expressions when clearer
(= total (reduce + 0 x))  # Sometimes prefix is better!

# Comprehensions with Pythonic calls
evens = [x for x in x if (== (% x 2) 0)]
print(f"Evens: {evens}")
print(f"Sum: {sum(evens)}")
```

### Nested Calls
```python
# All of these work!
print(len(range(10)))
print(sum(list(range(5))))
print(sorted(list(reversed([1, 2, 3]))))
```

## Implementation

**Single parser change** in `parser/parser.go`:

When parsing `TOKEN_IDENTIFIER`, check if next token is `TOKEN_LPAREN` with no whitespace:
- If yes: Parse as function call `name(args)` → `(name args)`
- If no: Just a symbol

**30 comprehensive tests** in `tests/pythonic-function-calls-test.m28`

**All existing tests pass** - 100% backward compatible!

## Benefits

✅ **Shell scripting**: Python-like one-liners without whitespace sensitivity
✅ **Readability**: Choose the clearest style for each situation
✅ **Familiarity**: Looks like Python for simple scripts
✅ **Power**: S-expressions still available for macros and metaprogramming
✅ **Compatibility**: All existing code still works

## Phase 2: Assignment Sugar ✅ IMPLEMENTED

**Syntax**: `x = value` transforms to `(= x value)`

### Examples

```python
# Basic assignment
x = 10
name = "Alice"
data = [1, 2, 3]

# With function calls
total = sum([1, 2, 3, 4, 5])
length = len(data)

# With comprehensions
squares = [x*x for x in range(10)]
evens = [x for x in range(100) if (== (% x 2) 0)]

# Chained assignment
x = y = z = 42

# Mix styles
pythonic = 10
(= sexp 20)
```

### Shell One-Liners

```bash
m28 -e 'x = 10 print(x)'
m28 -e 'data = [1,2,3] print(sum(data))'
m28 -e 'nums = range(10) evens = [x for x in nums if (== (% x 2) 0)] print(evens)'
```

### Implementation

**Single parser change** in `parser/parser.go`:

In `parseExpr()`, before `parseAtomFromToken()`, detect pattern: `IDENTIFIER = expr`
- Look ahead for `TOKEN_ASSIGN` (not `TOKEN_EQ` which is `==`)
- Transform to `(= identifier value)`

**30 comprehensive tests** in `tests/pythonic-assignment-test.m28`

**All tests pass** - 100% backward compatible!

## Phase 3: Function Definition Sugar ✅ IMPLEMENTED

**Syntax**: `def name(params): expr` transforms to `(def name (params) expr)`

### Examples

```python
# Zero parameters
def get_pi(): 3.14159

# Single parameter
def double(x): (* x 2)

# Multiple parameters
def add(a, b): (+ a b)
def greet(first, last): f"Hello, {first} {last}!"

# With comprehensions
def squares(n): [x*x for x in range(n)]
def evens(nums): [x for x in nums if (== (% x 2) 0)]

# Recursive
def factorial(n): (if (<= n 1) 1 (* n (factorial (- n 1))))

# Higher-order
def apply_twice(f, x): f(f(x))

# S-expression style still works!
(def sexp_multiply (x y) (* x y))
```

### Key Feature: Whitespace Distinguishes Styles

**Pythonic**: No whitespace between name and `(`
```python
def double(x): x * 2        # Pythonic - triggers sugar
```

**S-expression**: Whitespace before `(`
```lisp
(def double (x) (* x 2))    # S-expression - no sugar
```

This allows both styles to coexist without conflict!

### Shell One-Liners

```bash
m28 -e 'def double(x): x*2 print(double(5))'
m28 -e 'def squares(n): [x*x for x in range(n)] print(squares(5))'
```

### Implementation

**Pattern detection** in `parseAtomFromToken()`:

When we see identifier "def":
1. Check if followed by identifier
2. Check if that identifier is followed by `(` with NO whitespace
3. If yes: Parse as Pythonic def
4. If no: Return "def" as symbol for S-expression

**30 comprehensive tests** in `tests/pythonic-def-test.m28`

**All tests pass** - 100% backward compatible!

## Testing

```bash
# Run comprehensive tests
./bin/m28 tests/pythonic-function-calls-test.m28  # Phase 1
./bin/m28 tests/pythonic-assignment-test.m28      # Phase 2
./bin/m28 tests/pythonic-def-test.m28             # Phase 3

# Run example demos
./bin/m28 examples/pythonic-syntax-demo.m28
./bin/m28 examples/pythonic-assignment-demo.m28
./bin/m28 examples/pythonic-def-demo.m28

# Try it yourself!
./bin/m28 -e 'print("Hello, Pythonic M28!")'
./bin/m28 -e 'x = 10 print(x)'
./bin/m28 -e 'def double(x): x*2 print(double(5))'
```

## Summary

**All 3 Phases Complete!** 🎉

M28 is now truly **"Pythonic S-expressions"** - the best of both worlds:

### What We Built
1. ✅ **Phase 1**: Function calls `print(x)` → `(print x)`
2. ✅ **Phase 2**: Assignment `x = 10` → `(= x 10)`
3. ✅ **Phase 3**: Function defs `def f(x): expr` → `(def f (x) expr)`

### Complete Python-like Syntax
```python
# Now you can write pure Python-style M28!
x = 10
data = [1, 2, 3, 4, 5]

def double(x): x * 2
def filter_evens(nums): [x for x in nums if (== (% x 2) 0)]

evens = filter_evens(data)
doubled = [double(x) for x in evens]
print(f"Result: {doubled}")
```

### Benefits
- ✅ Python-like syntax for everyday scripting
- ✅ S-expressions still available for metaprogramming power
- ✅ Perfect for shell one-liners
- ✅ No mandatory whitespace/indentation
- ✅ Mix styles freely
- ✅ 100% backward compatible
