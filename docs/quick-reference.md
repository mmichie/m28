# M28 Quick Reference

## Syntax Rules
```lisp
# Three Golden Rules:
# 1. Comments: Always use # (never ;)
# 2. Variables: Always use = for assignment
# 3. Functions: Only use def for function definitions
# 4. Everything is prefix notation: (function arg1 arg2)
```

## Variables
```lisp
# All variables use = for assignment
(= x 10)
(= name "Alice")
(= pi 3.14159)

# Multiple assignment
(= a b 5)              # a = 5, b = 5
(= x y (+ 2 3))        # x = 5, y = 5
```

## Functions
```lisp
# Function definition (only use def for functions!)
(def add (a b)
  (+ a b))

# With default parameters
(def greet (name "World")
  (print f"Hello, {name}!"))

# Variadic functions
(def sum (*args)
  (reduce + args 0))

# Lambda functions
(= double (lambda (x) (* x 2)))

# Function call
(add 2 3)              # Returns 5
(greet)                # Prints: Hello, World!
(greet "Alice")        # Prints: Hello, Alice!
```

## Data Types
```lisp
# Numbers
42                     # Integer
3.14                   # Float

# Strings
"Hello"                # String
f"Value: {x}"          # F-string

# Booleans
True, true             # Boolean true
False, false           # Boolean false

# None
None, nil              # Null value

# Lists (mutable)
[1, 2, 3]              # List literal
(list 1 2 3)           # List function

# Tuples (immutable)
(1, 2, 3)              # Tuple literal
(tuple 1 2 3)          # Tuple function

# Dictionaries
{"key": "value", "num": 42}    # Dict literal
(dict "key" "value" "num" 42)  # Dict function

# Sets
{1, 2, 3}              # Set literal
(set 1 2 3)            # Set function
```

## Operators
```lisp
# Arithmetic (all prefix notation)
(+ 1 2 3)              # 6 (addition)
(- 10 3)               # 7 (subtraction)
(* 2 3 4)              # 24 (multiplication)
(/ 10 2)               # 5 (division)
(// 10 3)              # 3 (floor division)
(% 10 3)               # 1 (modulo)
(** 2 3)               # 8 (power)

# Comparison (use == for equality in conditions!)
(== a b)               # Equal
(!= a b)               # Not equal
(< a b)                # Less than
(<= a b)               # Less or equal
(> a b)                # Greater than
(>= a b)               # Greater or equal

# Logical
(and a b c)            # All must be true
(or a b c)             # Any must be true
(not a)                # Negation

# Membership
(in item collection)   # Check membership
```

## Control Flow
```lisp
# if expression (single line)
(if (> x 0) "positive" "non-positive")

# if statement (multi-line)
(if (> x 0)
  (print "positive")
  (print "non-positive"))

# if-elif-else
(if (< x 0)
  (print "negative")
  (elif (== x 0)
    (print "zero")
    (print "positive")))

# while loop
(while (< i 10)
  (print i)
  (= i (+ i 1)))

# for loop - NOT SUPPORTED
# Use while loops instead
```

## Data Access
```lisp
# List/Tuple access
(nth lst 0)            # First element
(nth lst -1)           # Last element
lst[0]                 # Index notation (assignment only)

# Dictionary access
dict.key               # Dot notation (read only)
(dict.get "key")       # Get method
(dict.get "key" default)  # With default

# String methods
(str.upper)            # Convert to uppercase
(str.split)            # Split into list
(str.replace "old" "new")  # Replace substring

# List methods (return new lists)
(+ lst [4])            # Concatenate lists
(len lst)              # Length
```

## List Operations
```lisp
# Lists in M28 are immutable for methods
# Methods return new lists, don't modify in-place

# Assignment (mutable)
(= lst[0] 99)          # Index assignment
(= lst[1:3] [a, b])    # Slice assignment

# Concatenation
(+ [1, 2] [3, 4])      # Returns [1, 2, 3, 4]

# Common operations
(len lst)              # Length
(map func lst)         # Map function over list
(filter pred lst)      # Filter list
(reduce func lst init) # Reduce list
```

## Dictionary Operations
```lisp
# Access
dict.key               # Dot notation
(dict.get "key")       # Get method
(dict.get "key" default)  # With default

# Methods
(dict.keys)            # Get keys
(dict.values)          # Get values
(dict.items)           # Get key-value pairs
(dict.pop "key")       # Remove and return
(dict.pop "key" default)  # With default
```

## String Operations
```lisp
# Methods
(str.upper)            # Uppercase
(str.lower)            # Lowercase
(str.strip)            # Remove whitespace
(str.split)            # Split to list
(str.split " ")        # Split by delimiter
(str.replace "old" "new")  # Replace
(" ".join lst)         # Join list elements

# F-strings
f"Hello, {name}!"      # Variable interpolation
f"{x + y}"             # Expression interpolation
```

## Exception Handling
```lisp
# Basic try-except
(try
  (risky-operation)
  (except
    (print "Error occurred")))

# Catch specific exception type
(try
  (/ 1 0)
  (except e
    (print "Error:" e)))

# With finally
(try
  (open-file)
  (except e
    (print "Error:" e))
  (finally
    (cleanup)))

# Raise exception
(raise "Error message")
```

## Modules
```lisp
# Import module
(import "math")                    # Import module
(import "path/to/module" as mod)   # Import with alias

# Use imported module
(math.sqrt 16)                     # Call module function
math.pi                            # Access module constant

# Module exports (in module file)
(= __exports__ (list "func1" "var1" "Class1"))
```

## Classes
```lisp
# Class definition
(class Point ()
  (def __init__ (self x y)
    (= self.x x)
    (= self.y y))
  
  (def distance (self)
    (math.sqrt (+ (** self.x 2) (** self.y 2)))))

# Create instance
(= p (Point 3 4))

# Access attributes and methods
p.x                    # Get attribute
(p.distance)           # Call method
```

## Common Patterns
```lisp
# Read file
(with (open "file.txt") as f
  (= content (f.read)))

# Write file
(with (open "file.txt" "w") as f
  (f.write "content"))

# List comprehension
[x * 2 for x in [1, 2, 3]]        # [2, 4, 6]
[x for x in lst if (> x 0)]       # Filter positive

# Main guard
(if (== __name__ "__main__")
  (main))
```

## Built-in Functions
```lisp
# I/O
(print "text" {"end": "\n", "sep": " "})
(input "prompt> ")

# Type conversion
(int "42")             # String to int
(str 42)               # To string
(float "3.14")         # To float
(bool x)               # To boolean
(list x)               # To list
(dict)                 # Empty dict

# Math
(abs -5)               # Absolute value
(round 3.7)            # Round to nearest
(min 1 2 3)            # Minimum
(max 1 2 3)            # Maximum
(sum [1, 2, 3])        # Sum of list

# Sequences
(range 5)              # [0, 1, 2, 3, 4]
(range 1 5)            # [1, 2, 3, 4]
(range 0 10 2)         # [0, 2, 4, 6, 8]
(len sequence)         # Length
(nth sequence index)   # Get item at index

# Type checking
(type obj)             # Get type
(isinstance obj type)  # Check type
```

## Special Variables
```lisp
__name__               # Module name or "__main__"
__file__               # Current file path
__exports__            # List of exported names
```

## Important Differences from Python

1. **Everything is prefix notation**: `(+ 1 2)` not `1 + 2`
2. **Use == in conditions**: `(if (== x 5) ...)` not `(if (= x 5) ...)`
3. **No for loops**: Use while loops or map/filter
4. **List methods return new lists**: They don't modify in place
5. **Dict access**: Use `dict.key` or `(dict.get "key")`, not `dict["key"]` for reading
6. **Import syntax**: Use `(import "module" as name)` not `(import "module" :as name)`