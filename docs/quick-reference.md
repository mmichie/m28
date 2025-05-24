# M28 Quick Reference

## Comments
```lisp
# Single line comment (always use #, never ;)
```

## Variables
```lisp
# Use = for ALL variables (never def)
(= x 10)
(= name "Alice")
(= pi 3.14159)
```

## Functions
```lisp
# Use def ONLY for functions
(def add (a b)
  (+ a b))

# With defaults
(def greet (name="World")
  (print f"Hello, {name}!"))

# Lambda
(= double (lambda (x) (* x 2)))
```

## Data Types
```lisp
# Numbers
(= int 42)
(= float 3.14)

# Strings
(= str "Hello")
(= fstr f"Value: {x}")

# Booleans
(= t True)   # or true
(= f False)  # or false

# None
(= n None)   # or nil

# Lists
(= lst [1, 2, 3])

# Tuples
(= tup (1, 2, 3))

# Dictionaries
(= dict {"key": "value", "num": 42})

# Sets
(= s {1, 2, 3})
```

## Operators
```lisp
# Arithmetic (prefix notation)
(+ 1 2 3)      # 6
(- 10 3)       # 7
(* 2 3 4)      # 24
(/ 10 2)       # 5
(% 10 3)       # 1
(** 2 3)       # 8

# Comparison
(== a b)       # Equality
(!= a b)       # Inequality
(< a b)        # Less than
(<= a b)       # Less or equal
(> a b)        # Greater than
(>= a b)       # Greater or equal

# Logical
(and a b c)    # All true
(or a b c)     # Any true
(not a)        # Negation

# Membership
(in item collection)
```

## Control Flow
```lisp
# if-else
(if condition
    then-expr
    else-expr)

# if-elif-else
(if cond1
    expr1
    (elif cond2
        expr2
        else-expr))

# while
(while condition
  body)

# for
(for item in collection
  body)

# for with unpacking
(for (k, v) in (items dict)
  body)

# break/continue
(break)
(continue)
```

## Data Access
```lisp
# List access
lst.0              # First element
(nth lst 2)        # Third element

# Dict access
dict.key           # Dot notation
(get dict "key")   # get function

# Method calls
obj.method(arg1, arg2)
```

## List Operations
```lisp
(append lst item)
(extend lst other-list)
(len lst)
(map func lst)
(filter pred lst)
(reduce func lst init)
```

## Dict Operations
```lisp
(keys dict)
(values dict)
(items dict)
(get dict key default)
(set dict key value)
```

## String Operations
```lisp
(split str sep)
(join sep lst)
(strip str)
(upper str)
(lower str)
(replace str old new)
(format str args...)
```

## Exceptions
```lisp
# try-except
(try
  risky-code
  (except ExceptionType as e
    handler))

# Multiple exceptions
(try
  code
  (except (Type1, Type2) as e
    handler)
  (finally
    cleanup))

# Raise
(raise (Exception "message"))
```

## Classes
```lisp
# Define class
(class ClassName (Parent)
  (def __init__ (self args...)
    body)
  
  (def method (self args...)
    body))

# Create instance
(= obj (ClassName args...))

# Call method
(obj.method args...)
```

## Modules
```lisp
# Import module
(import module)
(import module as alias)

# Import specific
(from module import func)
(from module import (f1, f2))

# Use imported
(module.function args...)
```

## Comprehensions
```lisp
# List comprehension
[expr for x in iterable]
[expr for x in iterable if condition]

# Dict comprehension
{k: v for (k, v) in items}

# Set comprehension
{expr for x in iterable}
```

## Common Patterns
```lisp
# Read file
(with (open "file.txt") as f
  (= content (f.read)))

# Write file
(with (open "file.txt" "w") as f
  (f.write "content"))

# Main guard
(if (== __name__ "__main__")
    (main))
```

## Built-in Functions
```lisp
# I/O
(print args...)
(input prompt)

# Type conversion
(int x)
(str x)
(float x)
(bool x)
(list x)
(tuple x)
(set x)
(dict pairs)

# Math
(abs x)
(round x)
(min args...)
(max args...)
(sum iterable)

# Iteration
(range stop)
(range start stop)
(range start stop step)
(enumerate iterable)
(zip iter1 iter2)

# Type checking
(type obj)
(isinstance obj type)
(hasattr obj attr)
(getattr obj attr default)
```

## Special Variables
```lisp
__name__     # Module name
__file__     # Current file
__all__      # Export list
self         # Instance reference
super        # Parent class
```