# M28 Language Guide

## Table of Contents
1. [Core Concepts](#core-concepts)
2. [Basic Syntax](#basic-syntax)
3. [Data Types](#data-types)
4. [Variables and Functions](#variables-and-functions)
5. [Control Flow](#control-flow)
6. [Data Structures](#data-structures)
7. [Object-Oriented Programming](#object-oriented-programming)
8. [Functional Programming](#functional-programming)
9. [Exception Handling](#exception-handling)
10. [Modules and Imports](#modules-and-imports)
11. [Advanced Features](#advanced-features)

## Core Concepts

M28 is a Lispy-Pythonic language that combines:
- **S-expressions**: Everything is an expression in prefix notation
- **Python semantics**: Familiar keywords, operators, and idioms
- **Homoiconicity**: Code is data, data is code
- **Dynamic typing**: Variables don't have fixed types

### Key Principles
1. All function calls use prefix notation: `(function arg1 arg2)`
2. Comments use `#` (Python-style, not `;`)
3. `def` is only for functions, `=` is only for variables
4. Python keywords and built-ins are preferred

## Basic Syntax

### Comments
```lisp
# This is a comment
# Comments always use # (Python-style)
```

### Expressions
Everything in M28 is an expression:
```lisp
(+ 1 2)              # Returns 3
(print "Hello")      # Returns None, prints Hello
(if (> x 0) x 0)     # Returns x if positive, else 0
```

### Naming Conventions
- Variables: `snake_case`
- Functions: `snake_case`
- Classes: `PascalCase`
- Constants: `UPPER_SNAKE_CASE`
- Private: `_leading_underscore`

## Data Types

### Numbers
```lisp
(= integer 42)
(= float 3.14)
(= negative -10)
(= scientific 1.23e-4)
```

### Strings
```lisp
(= simple "Hello, World!")
(= multi-line "Line 1\nLine 2")
(= escaped "She said \"Hi\"")
(= formatted f"The answer is {answer}")
```

### Booleans
```lisp
(= yes True)    # or true
(= no False)    # or false
```

### None/Nil
```lisp
(= nothing None)    # or nil
```

## Variables and Functions

### Variable Assignment
```lisp
# Use = for ALL variable assignments
(= x 10)
(= name "Alice")
(= numbers [1, 2, 3])

# Multiple assignment
(= (x, y) (10, 20))
```

### Function Definition
```lisp
# def is ONLY for functions
(def greet (name)
  (print f"Hello, {name}!"))

# With default arguments
(def greet (name="World")
  (print f"Hello, {name}!"))

# With multiple return values
(def divmod (a b)
  ((// a b), (% a b)))

# Variadic functions
(def sum (*args)
  (reduce + args 0))

# Keyword arguments
(def configure (**kwargs)
  (print kwargs))
```

### Lambda Functions
```lisp
(= square (lambda (x) (* x x)))
(= add (lambda (x y) (+ x y)))

# Using lambdas with higher-order functions
(map (lambda (x) (* x 2)) [1, 2, 3])
```

## Control Flow

### Conditionals
```lisp
# Simple if
(if (> x 0)
    (print "positive"))

# if-else
(if (> x 0)
    (print "positive")
    (print "non-positive"))

# if-elif-else
(if (> x 0)
    (print "positive")
    (elif (< x 0)
        (print "negative")
        (print "zero")))

# Conditional expression
(= result (if (> x 0) "positive" "non-positive"))
```

### Loops

#### While Loop
```lisp
(= i 0)
(while (< i 10)
  (print i)
  (= i (+ i 1)))

# With break and continue
(while True
  (= x (input "Enter number: "))
  (if (== x "quit")
      (break))
  (if (< x 0)
      (continue))
  (print (* x x)))
```

#### For Loop
```lisp
# Iterate over list
(for x in [1, 2, 3]
  (print x))

# With range
(for i in (range 10)
  (print i))

# Dictionary iteration
(for (k, v) in (items dict)
  (print f"{k}: {v}"))

# With enumerate
(for (i, val) in (enumerate list)
  (print f"{i}: {val}"))
```

## Data Structures

### Lists
```lisp
# Creation
(= empty [])
(= numbers [1, 2, 3, 4, 5])
(= mixed [1, "two", 3.0, [4, 5]])

# Access
(print numbers.0)        # First element (dot notation)
(print (nth numbers 2))  # Third element

# Modification
(append numbers 6)
(= numbers.0 10)        # Update first element

# List comprehensions
(= squares [x**2 for x in (range 10)])
(= evens [x for x in numbers if (== (% x 2) 0)])
```

### Dictionaries
```lisp
# Creation
(= empty {})
(= person {"name": "Alice", "age": 30, "city": "NYC"})

# Access
(print person.name)              # Dot notation
(print (get person "age"))       # get function
(print (get person "job" None))  # With default

# Modification
(= person.age 31)
(= person.job "Engineer")

# Dictionary comprehensions
(= squares {x: x**2 for x in (range 5)})

# Methods
(print (keys person))
(print (values person))
(print (items person))
```

### Sets
```lisp
# Creation
(= empty (set))
(= numbers {1, 2, 3})
(= from_list (set [1, 2, 2, 3, 3]))  # {1, 2, 3}

# Operations
(add numbers 4)
(remove numbers 2)
(print (in 3 numbers))  # True

# Set operations
(= a {1, 2, 3})
(= b {2, 3, 4})
(print (union a b))        # {1, 2, 3, 4}
(print (intersection a b)) # {2, 3}
(print (difference a b))   # {1}
```

### Tuples
```lisp
# Creation
(= empty ())
(= point (10, 20))
(= single (42,))  # Single element tuple

# Access (immutable)
(print point.0)   # 10
(print point.1)   # 20

# Unpacking
(= (x, y) point)
```

## Object-Oriented Programming

### Classes
```lisp
# Basic class
(class Animal
  (def __init__ (self name)
    (= self.name name))
  
  (def speak (self)
    (print f"{self.name} makes a sound")))

# Inheritance
(class Dog (Animal)
  (def __init__ (self name breed)
    (super.__init__ name)
    (= self.breed breed))
  
  (def speak (self)
    (print f"{self.name} barks!"))
  
  (def fetch (self)
    (print f"{self.name} fetches the ball")))

# Usage
(= rex (Dog "Rex" "Labrador"))
(rex.speak)       # Rex barks!
(rex.fetch)       # Rex fetches the ball
```

### Properties and Methods
```lisp
(class Person
  (def __init__ (self name age)
    (= self.name name)
    (= self._age age))  # Private by convention
  
  # Property getter
  (def age (self)
    self._age)
  
  # Property setter
  (def set_age (self value)
    (if (>= value 0)
        (= self._age value)
        (raise (ValueError "Age must be non-negative"))))
  
  # Class method
  (classmethod
  (def from_birth_year (cls name birth_year)
    (= age (- (current_year) birth_year))
    (cls name age)))
  
  # Static method  
  (staticmethod
  (def is_adult (age)
    (>= age 18))))
```

## Functional Programming

### Higher-Order Functions
```lisp
# map
(= numbers [1, 2, 3, 4, 5])
(= doubled (map (lambda (x) (* x 2)) numbers))

# filter
(= evens (filter (lambda (x) (== (% x 2) 0)) numbers))

# reduce
(= sum (reduce + numbers))
(= product (reduce * numbers 1))

# Custom higher-order function
(def apply_twice (f x)
  (f (f x)))

(print (apply_twice (lambda (x) (* x 2)) 5))  # 20
```

### Function Composition
```lisp
(def compose (f g)
  (lambda (x) (f (g x))))

(= add1 (lambda (x) (+ x 1)))
(= double (lambda (x) (* x 2)))
(= add1_then_double (compose double add1))

(print (add1_then_double 5))  # 12
```

### Partial Application
```lisp
(def partial (f *args)
  (lambda (*more_args)
    (apply f (+ args more_args))))

(= add (lambda (x y) (+ x y)))
(= add5 (partial add 5))
(print (add5 10))  # 15
```

## Exception Handling

### Try-Except
```lisp
(try
  (= result (/ 10 0))
  (except ZeroDivisionError as e
    (print f"Error: {e}")
    (= result 0)))

# Multiple exception types
(try
  (risky_operation)
  (except (ValueError, TypeError) as e
    (print f"Value or Type error: {e}"))
  (except Exception as e
    (print f"Unexpected error: {e}")))

# With else and finally
(try
  (= f (open "data.txt"))
  (= data (read f))
  (except IOError
    (print "Could not read file"))
  (else
    (print "File read successfully"))
  (finally
    (if (exists f) (close f))))
```

### Raising Exceptions
```lisp
(def divide (a b)
  (if (== b 0)
      (raise (ValueError "Cannot divide by zero"))
      (/ a b)))

# Custom exceptions
(class CustomError (Exception)
  (def __init__ (self message)
    (= self.message message)))

(raise (CustomError "Something went wrong"))
```

## Modules and Imports

### Importing Modules
```lisp
# Import entire module
(import math)
(print (math.sqrt 16))

# Import with alias
(import numpy as np)
(= arr (np.array [1, 2, 3]))

# Import specific items
(from math import (sqrt, pi))
(print (sqrt 16))

# Import all (use sparingly)
(from math import *)
```

### Creating Modules
```lisp
# In my_module.m28
(def public_function (x)
  (* x 2))

(def _private_function (x)
  (* x 3))

(class MyClass
  (def __init__ (self)
    (= self.value 42)))

# Module-level code
(print "Module loaded")

# Export specific items
(= __all__ ["public_function", "MyClass"])
```

## Advanced Features

### Generators
```lisp
(def fibonacci ()
  (= (a, b) (0, 1))
  (while True
    (yield a)
    (= (a, b) (b, (+ a b)))))

(= fib (fibonacci))
(for _ in (range 10)
  (print (next fib)))
```

### Context Managers
```lisp
# Using with statement
(with (open "file.txt" "r") as f
  (for line in f
    (print (strip line))))

# Custom context manager
(class Timer
  (def __enter__ (self)
    (= self.start (time))
    self)
  
  (def __exit__ (self exc_type exc_val exc_tb)
    (= self.end (time))
    (print f"Elapsed: {(- self.end self.start)}s")))

(with (Timer) as timer
  (sleep 1))
```

### Decorators (Future)
```lisp
(def memoize (f)
  (= cache {})
  (lambda (*args)
    (if (in args cache)
        cache.args
        (do
          (= result (f *args))
          (= cache.args result)
          result))))

(@memoize
(def fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
```

### Async/Await (Future)
```lisp
(async def fetch_data (url)
  (= response (await (http.get url)))
  response.json)

(async def main ()
  (= data (await (fetch_data "https://api.example.com")))
  (print data))

(run (main))
```

## Best Practices

1. **Use Python idioms**: Write code that feels Pythonic despite the s-expressions
2. **Prefer immutability**: Use tuples over lists when data won't change
3. **Keep functions small**: Each function should do one thing well
4. **Use descriptive names**: `calculate_total` not `calc` or `ct`
5. **Handle errors gracefully**: Use try-except for expected failures
6. **Document your code**: Use docstrings for functions and classes
7. **Follow PEP 8 spirit**: Even in s-expression syntax, maintain readability

## Common Patterns

### Builder Pattern
```lisp
(class QueryBuilder
  (def __init__ (self)
    (= self.query ""))
  
  (def select (self *fields)
    (= self.query (+ "SELECT " (join fields ", ")))
    self)
  
  (def from (self table)
    (= self.query (+ self.query f" FROM {table}"))
    self)
  
  (def where (self condition)
    (= self.query (+ self.query f" WHERE {condition}"))
    self)
  
  (def build (self)
    self.query))

(= query ((QueryBuilder)
          .select("name", "age")
          .from("users")
          .where("age > 18")
          .build))
```

### Factory Pattern
```lisp
(def create_shape (shape_type **kwargs)
  (if (== shape_type "circle")
      (Circle kwargs.radius)
      (elif (== shape_type "rectangle")
          (Rectangle kwargs.width kwargs.height)
          (raise (ValueError f"Unknown shape: {shape_type}")))))
```

This guide covers the essential aspects of M28. For more examples and patterns, see the [examples directory](../examples/).